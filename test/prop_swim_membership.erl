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

-define(SUT, swim_membership).
-define(LOCAL_MEMBER, {{127,0,0,1}, 5000}).

-export([command/1, initial_state/0, next_state/3, postcondition/3,
         precondition/2]).

-type member_status() :: alive | suspect | faulty.
-type incarnation() :: non_neg_integer().
-type member() :: {inet:ip_address(), inet:port_number()}.

-record(state, {
          me               :: member(),
          incarnation = 0  :: incarnation(),
          members     = [] :: [{member(), member_status(), incarnation()}]
         }).

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

prop_membership() ->
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
