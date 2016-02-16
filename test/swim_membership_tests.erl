-module(swim_membership_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(LOCAL_MEMBER, {{127,0,0,1}, 5000}).
-define(SUT, swim_membership).

-behavior(proper_statem).

-export([command/1, initial_state/0, next_state/3, postcondition/3,
	 precondition/2]).

-type member_status() :: alive | suspect | faulty.
-type incarnation() :: non_neg_integer().
-type member() :: {inet:ip_address(), inet:port_number()}.

-record(state, {
	  me :: member(),
	  incarnation = 0 :: incarnation(),
	  members = [] :: [{member(), member_status(), incarnation()}]
	 }).

membership_v2_local_member_test() ->
    {ok, EventMgrPid} = gen_event:start_link(),
    {ok, Membership} = swim_membership:start_link(?LOCAL_MEMBER, EventMgrPid, []),
    ?assertMatch(?LOCAL_MEMBER, swim_membership:local_member(Membership)).

membership_v2_suspect_timeout_test() ->
    {ok, EventMgrPid} = gen_event:start_link(),
    Seed = {{10,10,10,10}, 5000},
    {ok, Membership} = swim_membership:start_link(?LOCAL_MEMBER, EventMgrPid,
						  [{suspicion_factor, 1},
						   {protocol_period, 100},
						   {seeds, [Seed]}]),
    _ = swim_membership:suspect(Membership, Seed, 1),
    ok = timer:sleep(100),
    Members = [M || {M, _, _} <- swim_membership:members(Membership)],
    ok = gen_event:stop(EventMgrPid),
    ?assertNot(lists:member(Seed, Members)).

membership_v2_suspect_timeout_refute_test() ->
    {ok, EventMgrPid} = gen_event:start_link(),
    Seed = {{10,10,10,10}, 5000},
    {ok, Membership} = swim_membership:start_link(?LOCAL_MEMBER, EventMgrPid,
						  [{suspicion_factor, 10},
						   {protocol_period, 5000},
						   {seeds, [Seed]}]),
    _ = swim_membership:suspect(Membership, Seed, 1),
    _ = swim_membership:alive(Membership, Seed, 2),
    Members = swim_membership:members(Membership),
    ok = gen_event:stop(EventMgrPid),
    ?assert(lists:member({Seed, alive, 2}, Members)).

membership_v2_test_() ->
    {timeout, 60,
     ?_assert(proper:quickcheck(prop_membership_v2(), [{numtests, 500}, {to_file, user}]))}.

g_ipv4_address() ->
    tuple([integer(0, 255) || _ <- lists:seq(0, 3)]).

g_ipv6_address() ->
    tuple([integer(0, 65535) || _ <- lists:seq(0, 7)]).

g_ip_address() ->
    oneof([g_ipv4_address(), g_ipv6_address()]).

g_port_number() ->
    integer(0, 65535).

g_incarnation() ->
    integer(0, inf).

g_new_member() ->
    {g_ip_address(), g_port_number()}.

g_local_member(State) ->
    exactly(State#state.me).

g_non_local_member(State) ->
    ?LET({Member, _CurrentStatus, _CurrentInc},
	 oneof(State#state.members),
	 Member).

g_existing_member(State) ->
    oneof([g_local_member(State), g_non_local_member(State)]).

g_member(State) ->
    frequency([{1, g_new_member()}] ++
	      [{3, g_existing_member(State)} || State#state.members /= []]).

initial_state() ->
    #state{me=?LOCAL_MEMBER}.

command(State) ->
    oneof([
	   {call, ?SUT, alive, [{var, sut}, g_member(State), g_incarnation()]},
	   {call, ?SUT, suspect, [{var, sut}, g_member(State), g_incarnation()]},
	   {call, ?SUT, faulty, [{var, sut}, g_member(State), g_incarnation()]},
	   {call, ?SUT, members, [{var, sut}]}
	  ]).

precondition(_State, _Call) ->
    true.

postcondition(S, {call, _Mod, members, _Args}, Members) ->
    #state{members=KnownMembers} = S,
    ordsets:subtract(ordsets:from_list(KnownMembers),
		     ordsets:from_list(Members)) == [];
postcondition(_State, {call, _Mod, _, _Args}, _R) ->
    true.

next_state(S, _V, {call, _Mod, members, _Args}) ->
    S;
next_state(S, _V, {call, _Mod, alive, [_Pid, Member, Incarnation]}) ->
    #state{members=KnownMembers, incarnation=LocalIncarnation} = S,
    case S#state.me == Member of
	true ->
	    case Incarnation > LocalIncarnation of
		true ->
		    S#state{incarnation=Incarnation + 1};
		false ->
		    S
	    end;
	false ->
	    case lists:keytake(Member, 1, KnownMembers) of
		false ->
		    NewMembers = [{Member, alive, Incarnation} | KnownMembers],
		    S#state{members=NewMembers};
		{value, {Member, _CurrentStatus, CurrentIncarnation}, Rest}
		  when Incarnation > CurrentIncarnation ->
		    NewMembers = [{Member, alive, Incarnation} | Rest],
		    S#state{members=NewMembers};
		_ ->
		    S
	    end
    end;
next_state(#state{me=Me} = S, _V, {call, _Mod, suspect, [_Pid, Me, Incarnation]}) ->
    #state{incarnation=LocalIncarnation} = S,
    case Incarnation >= LocalIncarnation of
	true ->
	    S#state{incarnation=Incarnation + 1};
	false ->
	    S
    end;
next_state(S, _V, {call, _Mod, suspect, [_Pid, Member, Incarnation]}) ->
    #state{members=KnownMembers} = S,
    case lists:keytake(Member, 1, KnownMembers) of
	false ->
	    S;
	{value, {Member, _CurrentStatus, CurrentIncarnation}, Rest}
	  when Incarnation >= CurrentIncarnation ->
	    NewMembers = [{Member, suspect, Incarnation} | Rest],
	    S#state{members=NewMembers};
	_ ->
	    S
    end;
next_state(#state{me=Me} = S, _V, {call, _Mod, faulty, [_, Me, Incarnation]}) ->
    #state{incarnation=LocalIncarnation} = S,
    case Incarnation >= LocalIncarnation of
	true ->
	    S#state{incarnation=Incarnation + 1};
	false ->
	    S
    end;
next_state(S, _V, {call, _Mod, faulty, [_Pid, Member, Incarnation]}) ->
    #state{members=KnownMembers} = S,
    case lists:keytake(Member, 1, KnownMembers) of
	false ->
	    S;
	{value, {Member, _CurrentStatus, CurrentIncarnation}, Rest}
	  when Incarnation >= CurrentIncarnation ->
	    S#state{members=Rest};
	_ ->
	    S
    end.

prop_membership_v2() ->
    ?FORALL(Cmds, commands(?MODULE),
	    ?TRAPEXIT(
	       begin
		   {ok, EventMgrPid} = gen_event:start_link(),
		   {ok, Pid} = ?SUT:start_link(?LOCAL_MEMBER, EventMgrPid,
					       [{suspicion_factor, 1},
						{protocol_period, 1}]),
		   {_H, _S, R} = run_commands(?MODULE, Cmds, [{sut, Pid}]),
		   ok = gen_event:stop(EventMgrPid),
		   ?WHENFAIL(
		      io:format("Result: ~p\n", [R]),
		      aggregate(command_names(Cmds), R =:= ok))
	       end)).
