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

-export([command/1]).
-export([initial_state/0]).
-export([next_state/3]).
-export([postcondition/3]).
-export([precondition/2]).

-export([alive/2]).
-export([suspect/3]).
-export([faulty/3]).
-export([members/0]).

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
    exactly(State#state.me).

g_non_local_member(State) ->
    ?LET({Member, _CurrentStatus, _CurrentInc},
         oneof(State#state.members),
         Member).

g_existing_member(State) ->
    oneof([g_local_member(State), g_non_local_member(State)]).

g_suspecting_member(State) ->
    frequency([{5, g_member(State)}, {1, local}]).

g_member(State) ->
    frequency([{1, g_member()}] ++
                  [{3, g_existing_member(State)} || State#state.members /= []]).

initial_state() ->
    #state{me = {{127,0,0,1},5000}}.

command(State) ->
    oneof([
           {call, ?MODULE, alive, [g_member(State), g_incarnation()]},
           {call, ?MODULE, suspect,
            [g_member(State), g_incarnation(), g_suspecting_member(State)]},
           {call, ?MODULE, faulty,
            [g_member(State), g_incarnation(), g_suspecting_member(State)]},
           {call, ?MODULE, members, []}
          ]).

precondition(_State, {call, ?MODULE, suspect, [Member, _Inc, Member]}) ->
    false;
precondition(_State, {call, ?MODULE, faulty, [Member, _Inc, Member]}) ->
    false;
precondition(_State, _Call) ->
    true.

postcondition(State, {call, ?MODULE, members, []}, Members) ->
    ordsets:subtract(ordsets:from_list(State#state.members),
                     ordsets:from_list(Members)) =:= [];
postcondition(_State, {call, ?MODULE, alive, [_Member, _Inc]}, ok) ->
    true;
postcondition(_State, {call, ?MODULE, suspect, [_Member, _Inc, _From]}, ok) ->
    true;
postcondition(_State, {call, ?MODULE, faulty, [_Member, _Inc, _From]}, ok) ->
    true.

next_state(State, _V, {call, ?MODULE, members, []}) ->
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
next_state(State, _V, {call, ?MODULE, suspect, [Member, Incarnation, _From]}) ->
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
next_state(State, _V, {call, ?MODULE, faulty, [Member, Incarnation, _From]}) ->
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
                {value, {Member, _Status, CurrentIncarnation}, Rest}
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

suspect(Member, Incarnation, From) ->
    gen_server:call(?MODULE, {suspect, Member, Incarnation, From}).

faulty(Member, Incarnation, From) ->
    gen_server:call(?MODULE, {faulty, Member, Incarnation, From}).

members() ->
    gen_server:call(?MODULE, members).

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
    {reply, Members, Membership}.

handle_cast(_Msg, Membership) ->
    {noreply, Membership}.

handle_info(_Info, Membership) ->
    {noreply, Membership}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

