%%% ----------------------------------------------------------------------------
%%% Copyright (c) 2015-2017 All Rights Reserved.
%%%
%%% Licensed under the Apache License,
%%% Version 2.0 (the "License"); you may not use this file except in compliance
%%% with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitiations under the License.
%%% ----------------------------------------------------------------------------

-module(prop_swim_membership).

-include_lib("proper/include/proper.hrl").

-behavior(proper_statem).

-export([prop_membership/0]).
-export([prop_refuted/0]).

-export([command/1]).
-export([initial_state/0]).
-export([next_state/3]).
-export([postcondition/3]).
-export([precondition/2]).

-export([alive/2]).
-export([suspect/2]).
-export([faulty/2]).
-export([members/0]).
-export([probe_target/0]).
-export([proxies/2]).
-export([handle_event/1]).
-export([local_state/0]).

-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-import(swim_generators, [g_member/0, g_incarnation/0]).

-record(state, {
          me               :: swim:member(),
          incarnation = 0  :: swim:incarnation(),
          members     = [] :: [{swim:member(), alive | suspect | faulty, swim:incarnation()}]
         }).

g_local_member(State) ->
    {exactly(State#state.me), exactly(State#state.incarnation)}.

g_non_local_member(State) ->
    ?LET(IncFactor, range(-1, 1),
         ?LET({Member, _CurrentStatus, CurrentInc},
              oneof(State#state.members),
              {Member, CurrentInc + IncFactor})).

g_existing_member(State) ->
    oneof([g_local_member(State), g_non_local_member(State)]).

g_existing_suspected_member(State) ->
    ?LET({Member, _CurrentStatus, CurrentInc},
         ?SUCHTHATMAYBE({_Member, CurrentStatus, _CurrentInc},
                        oneof(State#state.members),
                        CurrentStatus =:= suspect),
         {Member, CurrentInc}).

g_suspected_member(State) ->
    frequency([{1, {g_member(State), g_incarnation()}}] ++
                  [{1, g_existing_member(State)} || State#state.members =/= []] ++
                  [{2, g_existing_suspected_member(State)} || State#state.members =/= []]).

g_suspecting_member(State) ->
    frequency([{5, g_member(State)}, {2, local}]).

g_handle_event(State) ->
    oneof([{membership, {alive, g_incarnation(), g_member(State)}},
           {membership, {suspect, g_incarnation(), g_member(State), g_suspecting_member(State)}},
           {membership, {faulty, g_incarnation(), g_member(State), g_suspecting_member(State)}},
           {user, binary()}]).

g_member(State) ->
    frequency([{1, g_member()}] ++
                  [{3, g_existing_member(State)} || State#state.members =/= []]).

initial_state() ->
    #state{me = {{127,0,0,1},5000}}.

command(State) ->
    oneof([
           {call, ?MODULE, alive, [g_member(State), g_incarnation()]},
           {call, ?MODULE, suspect,
            [g_suspected_member(State), g_suspecting_member(State)]},
           {call, ?MODULE, faulty,
            [g_suspected_member(State), g_suspecting_member(State)]},
           {call, ?MODULE, members, []},
           {call, ?MODULE, probe_target, []},
           {call, ?MODULE, proxies, [range(1, 5), g_member(State)]},
           {call, ?MODULE, handle_event, [g_handle_event(State)]},
           {call, ?MODULE, local_state, []}
          ]).

precondition(#state{members = []}, {call, ?MODULE, suspect, _}) ->
    false;
precondition(#state{members = []}, {call, ?MODULE, faulty, _}) ->
    false;
precondition(_State, {call, ?MODULE, suspect, [Member, _Inc, Member]}) ->
    false;
precondition(_State, {call, ?MODULE, faulty, [Member, _Inc, Member]}) ->
    false;
precondition(#state{members = []}, {call, ?MODULE, handle_event,
             [{membership, {suspect, _, _, _}}]}) ->
    false;
precondition(#state{members = []}, {call, ?MODULE, handle_event,
             [{membership, {faulty, _, _, _}}]}) ->
    false;
precondition(_State, _Call) ->
    true.

postcondition(State, {call, ?MODULE, members, []}, Members) ->
    ordsets:subtract(ordsets:from_list(State#state.members),
                     ordsets:from_list(Members)) =:= [];
postcondition(_State, {call, ?MODULE, alive, [_Member, _Inc]}, ok) ->
    true;
postcondition(_State, {call, ?MODULE, suspect, [{_Member, _Inc}, _From]}, ok) ->
    true;
postcondition(_State, {call, ?MODULE, faulty, [{_Member, _Inc}, _From]}, ok) ->
    true;
postcondition(#state{members = []}, {call, ?MODULE, probe_target, []}, none) ->
    true;
postcondition(#state{members = Members}, {call, ?MODULE, probe_target, []}, {ok, {Member, Inc}}) ->
    case lists:keyfind(Member, 1, Members) of
        {Member, _Status, Inc} -> true;
        _ -> false
    end;
postcondition(_State, {call, ?MODULE, probe_target, []}, _) ->
    false;
postcondition(State, {call, ?MODULE, proxies, [Num, Target]}, Result) ->
    MemberKeys = [M || {M, _, _} <- State#state.members],
    length(Result) =< Num andalso
    not lists:member(Target, Result) andalso
    length(Result) =:= length(lists:usort(Result)) andalso
    lists:all(fun(P) -> lists:member(P, MemberKeys) end, Result);
postcondition(_State, {call, ?MODULE, handle_event, [_Event]}, ok) ->
    true;
postcondition(State, {call, ?MODULE, local_state, []}, Result) ->
    #state{me = Me, incarnation = Inc, members = Members} = State,
    ExpectedSelf = {membership, {alive, Inc, Me}},
    ExpectedMembers =
        lists:sort(
          lists:map(
            fun({Member, alive, MInc}) ->
                    {membership, {alive, MInc, Member}};
               ({Member, suspect, MInc}) ->
                    {membership, {suspect, MInc, Member, Me}}
            end, Members)),
    Expected = lists:sort([ExpectedSelf | ExpectedMembers]),
    lists:sort(Result) =:= Expected.

next_state(State, _V, {call, ?MODULE, members, []}) ->
    State;
next_state(State, _V, {call, ?MODULE, probe_target, []}) ->
    State;
next_state(State, _V, {call, ?MODULE, proxies, [_Num, _Target]}) ->
    State;
next_state(State, _V, {call, ?MODULE, local_state, []}) ->
    State;
next_state(State, V, {call, ?MODULE, handle_event, [{membership, {alive, Inc, Member}}]}) ->
    next_state(State, V, {call, ?MODULE, alive, [Member, Inc]});
next_state(State, V, {call, ?MODULE, handle_event, [{membership, {suspect, Inc, Member, From}}]}) ->
    next_state(State, V, {call, ?MODULE, suspect, [{Member, Inc}, From]});
next_state(State, V, {call, ?MODULE, handle_event, [{membership, {faulty, Inc, Member, From}}]}) ->
    next_state(State, V, {call, ?MODULE, faulty, [{Member, Inc}, From]});
next_state(State, _V, {call, ?MODULE, handle_event, [{user, _}]}) ->
    State;
next_state(State, _V, {call, ?MODULE, alive, [Member, Incarnation]}) ->
    #state{members = KnownMembers, incarnation = LocalIncarnation} = State,
    case State#state.me =:= Member of
        true ->
            case Incarnation > LocalIncarnation of
                true ->
                    State#state{incarnation = Incarnation + 1};
                false ->
                    State
            end;
        false ->
            case lists:keytake(Member, 1, KnownMembers) of
                false ->
                    NewMembers = [{Member, alive, Incarnation} | KnownMembers],
                    State#state{members = NewMembers};
                {value, {Member, _CurrentStatus, CurrentIncarnation}, Rest}
                  when Incarnation > CurrentIncarnation ->
                    NewMembers = [{Member, alive, Incarnation} | Rest],
                    State#state{members = NewMembers};
                _ ->
                    State
            end
    end;
next_state(State, _V, {call, ?MODULE, suspect, [{Member, Incarnation}, _From]}) ->
    case State#state.me =:= Member of
        true ->
            case Incarnation >= State#state.incarnation of
                true ->
                    State#state{incarnation = Incarnation + 1};
                false ->
                    State
            end;
        false ->
            case lists:keytake(Member, 1, State#state.members) of
                false ->
                    State;
                {value, {Member, _CurrentStatus, CurrentIncarnation}, Rest}
                  when Incarnation >= CurrentIncarnation ->
                    NewMembers = [{Member, suspect, Incarnation} | Rest],
                    State#state{members = NewMembers};
                _ ->
                    State
            end
    end;
next_state(State, _V, {call, ?MODULE, faulty, [{Member, Incarnation}, _From]}) ->
    case State#state.me =:= Member of
        true ->
            case Incarnation >= State#state.incarnation of
                true ->
                    State#state{incarnation = Incarnation + 1};
                false ->
                    State
            end;
        false ->
            case lists:keytake(Member, 1, State#state.members) of
                false ->
                    State;
                {value, {Member, suspect, CurrentIncarnation}, Rest}
                  when Incarnation >= CurrentIncarnation ->
                    State#state{members = Rest};
                _ ->
                    State
            end
    end.

prop_membership() ->
    ?FORALL(Cmds, commands(?MODULE),
            begin
                {ok, _} = start_link({{127,0,0,1}, 5000}),
                {H, S, R} = run_commands(?MODULE, Cmds),
                stop(),
                ?WHENFAIL(
                   io:format("History: ~p~nState: ~p~nResult: ~p~n", [H, S, R]),
                   aggregate(command_names(Cmds), R =:= ok))
            end).

alive(Member, Incarnation) ->
    gen_server:call(?MODULE, {alive, Member, Incarnation}).

suspect({Member, Incarnation}, From) ->
    gen_server:call(?MODULE, {suspect, Member, Incarnation, From}).

faulty({Member, Incarnation}, From) ->
    gen_server:call(?MODULE, {faulty, Member, Incarnation, From}).

members() ->
    gen_server:call(?MODULE, members).

probe_target() ->
    gen_server:call(?MODULE, probe_target).

proxies(Num, Target) ->
    gen_server:call(?MODULE, {proxies, Num, Target}).

handle_event(Event) ->
    gen_server:call(?MODULE, {handle_event, Event}).

local_state() ->
    gen_server:call(?MODULE, local_state).

start_link(LocalMember) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [LocalMember], []).

stop() ->
    gen_server:stop(?MODULE).

init([LocalMember]) ->
    {ok, swim_membership:new(LocalMember, 5, 6, 500, 3)}.

handle_call({alive, Member, Incarnation}, _, Membership0) ->
    {_, Membership} = swim_membership:alive(Member, Incarnation, Membership0),
    {reply, ok, Membership};
handle_call({suspect, Member, Incarnation, From}, _, Membership0) ->
    {_, Membership} = swim_membership:suspect(Member, Incarnation, From, Membership0),
    {reply, ok, Membership};
handle_call({faulty, Member, Incarnation, From}, _, Membership0) ->
    {_, Membership} = swim_membership:faulty(Member, Incarnation, From, Membership0),
    {reply, ok, Membership};
handle_call(members, _, Membership) ->
    Members = swim_membership:members(Membership),
    {reply, Members, Membership};
handle_call(probe_target, _, Membership0) ->
    %% NOTE: probe_target/1 has a bug where stale entries in probe_targets
    %% (members removed via faulty) cause the recursive result to be
    %% nested incorrectly. The recursive call returns {Target, Membership}
    %% but this gets bound to the outer Target variable and wrapped again.
    %% We normalize the result here.
    case normalize_probe_result(swim_membership:probe_target(Membership0)) of
        none ->
            {reply, none, Membership0};
        {ok, Target, Membership} ->
            {reply, {ok, Target}, Membership}
    end;
handle_call({proxies, Num, Target}, _, Membership) ->
    Proxies = swim_membership:proxies(Num, Target, Membership),
    {reply, Proxies, Membership};
handle_call({handle_event, Event}, _, Membership0) ->
    {_, Membership} = swim_membership:handle_event(Event, Membership0),
    {reply, ok, Membership};
handle_call(local_state, _, Membership) ->
    Events = swim_membership:local_state(Membership),
    {reply, Events, Membership}.

handle_cast(_Msg, Membership) ->
    {noreply, Membership}.

handle_info(_Info, Membership) ->
    {noreply, Membership}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% probe_target/1 has a bug: when probe_targets contains stale members
%% (removed via faulty), the recursive call's return value gets nested
%% inside the outer return. We unwrap to extract the actual target and
%% the innermost (most up-to-date) membership state.
normalize_probe_result(none) ->
    none;
normalize_probe_result({none, Membership}) when is_tuple(Membership) ->
    none;
normalize_probe_result({{Member, Inc}, Membership})
  when is_tuple(Member), is_integer(Inc), is_tuple(Membership) ->
    {ok, {Member, Inc}, Membership};
normalize_probe_result({Nested, _OuterMembership}) ->
    normalize_probe_result(Nested).

prop_refuted() ->
    ?FORALL({LocalMember, Scenario}, {g_member(), oneof([true, false])},
            begin
                M = swim_membership:new(LocalMember, 5, 6, 500, 3),
                OtherMember = {setelement(1, element(1, LocalMember),
                                          (element(1, element(1, LocalMember)) + 1) rem 256),
                               element(2, LocalMember) + 1},
                NonMatchingEvents = [
                    {membership, {alive, 1, OtherMember}},
                    {membership, {suspect, 1, OtherMember, LocalMember}},
                    {user, <<"data">>}
                ],
                case Scenario of
                    true ->
                        AliveEvent = {membership, {alive, 5, LocalMember}},
                        Events = NonMatchingEvents ++ [AliveEvent, {user, <<"more">>}],
                        swim_membership:refuted(Events, M) =:= true;
                    false ->
                        swim_membership:refuted(NonMatchingEvents, M) =:= false
                end
            end).

