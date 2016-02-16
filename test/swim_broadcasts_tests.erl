-module(swim_broadcasts_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-behavior(proper_statem).

-export([command/1, initial_state/0, next_state/3, postcondition/3,
	 precondition/2]).

-record(state, {
	  events = [] :: [],
	  retransmit_factor :: pos_integer()
	 }).

-define(RETRANSMIT_FACTOR, 5).

g_member_status() ->
    oneof([alive, suspect, faulty]).

g_ip_address() ->
    oneof([
	   tuple([integer(0, 255) || _ <- lists:seq(0, 3)]),
	   tuple([integer(0, 65535) || _ <- lists:seq(0, 7)])
	  ]).

g_port_number() ->
    integer(0, 65535).

g_member() ->
    tuple([g_ip_address(), g_port_number()]).

g_incarnation() ->
    integer(0, inf).

g_membership_event() ->
    ?LET(Event,
	 {g_member_status(), g_member(), g_incarnation()},
	 Event).

swim_broadcasts_test_() ->
    {timeout, 60,
     ?_assert(proper:quickcheck(prop_swim_broadcasts(), [{to_file, user}]))}.

initial_state() ->
    #state{events=[], retransmit_factor=?RETRANSMIT_FACTOR}.

command(_State) ->
    oneof([
	   {call, swim_broadcasts, membership, [{var, sut}, g_membership_event()]},
	   {call, swim_broadcasts, dequeue, [{var, sut}, pos_integer()]}
	  ]).

precondition(_State, _Call) ->
    true.

next_state(S, _V, {call, _Mod, membership, [_, Event]}) ->
    #state{events=KnownEvents} = S,
    S#state{events=[{0, Event} | KnownEvents]};
next_state(S, _V, {call, _Mod, dequeue, [_, NumMembers]}) ->
    #state{events=KnownEvents, retransmit_factor=RetransmitFactor} = S,
    MaxTransmissions = swim_broadcasts:max_transmissions(NumMembers, RetransmitFactor),
    NewEvents = lists:filtermap(
		  fun({T, E}) ->
			  case T - MaxTransmissions of
			      R when R =< 0 ->
				  false;
			      R when R > 0 ->
				  {true, {R, E}}
			  end
		  end, KnownEvents),
    S#state{events=NewEvents};
next_state(S, _V, _Call) ->
    S.

postcondition(_S, {call, _Mod, membership, _Args}, _R) ->
    true;
postcondition(S, {call, _Mod, dequeue, _Args}, Broadcasts) ->
    #state{events=KnownEvents} = S,
    DecodedEvents = swim_messages:decode_events(Broadcasts),
    ordsets:subtract(ordsets:from_list([E || {_T, E} <- KnownEvents]),
		     ordsets:from_list([B || {membership, B} <- DecodedEvents])) == [].

prop_swim_broadcasts() ->
    ?FORALL(Cmds, commands(?MODULE),
	    ?TRAPEXIT(
	       begin
		   {ok, EventMgrPid} = gen_event:start_link(),
		   ok = gen_event:add_sup_handler(EventMgrPid, swim_broadcasts,
						  [?RETRANSMIT_FACTOR]),
		   {_H, _S, R} = run_commands(?MODULE, Cmds, [{sut, EventMgrPid}]),
		   ok = gen_event:stop(EventMgrPid),
		   ?WHENFAIL(
		      io:format("Result: ~p\n", [R]),
		      aggregate(command_names(Cmds), R =:= ok))
	       end)).
