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

-module(prop_swim_broadcasts).

-include_lib("proper/include/proper.hrl").

-compile([export_all]).

-export([prop_retransmit_limit/0]).
-export([prop_invalidation/0]).

-import(swim_generators, [swim_event/0, g_member/0, g_incarnation/0]).

-record(state, {
          events = [] :: {non_neg_integer(), swim:swim_event()},
          pruned = [] :: swim:membership_event()
         }).

g_retransmits() ->
    range(1, 10).

g_target_member(#state{events = Events}) ->
    SuspectMembers = [M || {_, {suspect, _, M, _}} <- Events],
    case SuspectMembers of
        [] -> g_member();
        _ -> frequency([{1, g_member()} | [{3, exactly(M)} || M <- SuspectMembers]])
    end.

g_suspect_insert() ->
    ?LET({Inc, Member, From}, {g_incarnation(), g_member(), g_member()},
         {membership, {suspect, Inc, Member, From}}).

initial_state() ->
    #state{events = [], pruned = []}.

command(State) ->
    frequency([
               {2, {call, ?MODULE, insert, [swim_event()]}},
               {2, {call, ?MODULE, insert, [g_suspect_insert()]}},
               {1, {call, ?MODULE, take, []}},
               {2, {call, ?MODULE, take_target, [g_target_member(State)]}},
               {1, {call, ?MODULE, prune, [g_retransmits()]}}
              ]).

precondition(#state{events = []}, {call, ?MODULE, take, _}) ->
    false;
precondition(#state{events = []}, {call, ?MODULE, take_target, _}) ->
    false;
precondition(#state{events = []}, {call, ?MODULE, prune, _}) ->
    false;
precondition(_State, _Call) ->
    true.

next_state(State, _V, {call, ?MODULE, insert, [Event]}) ->
    State#state{events = lists:sort(fun sort/2, [{0, Event} | State#state.events])};
next_state(State, _V, {call, ?MODULE, take, []}) ->
    N = min(length(State#state.events), 11),
    {Taken0, Rest} = lists:split(N, lists:sort(fun sort/2, State#state.events)),
    Taken = [{T + 1, E} || {T, E} <- Taken0],
    State#state{events = lists:sort(fun sort/2, Taken ++ Rest)};
next_state(State, _V, {call, ?MODULE, take_target, [Target]}) ->
    Partition = fun({_, {suspect, _, M, _}}) -> M =:= Target; (_) -> false end,
    {Maybe, Rest} = lists:partition(Partition, State#state.events),
    Sorted = lists:sort(fun sort/2, Rest),
    case Maybe of
        [] ->
            N = min(length(Sorted), 11),
            {Taken0, Remaining} = lists:split(N, Sorted),
            Taken = [{T + 1, E} || {T, E} <- Taken0],
            State#state{events = lists:sort(fun sort/2, Taken ++ Remaining)};
        [{T, About}] ->
            N = min(length(Sorted), 10),
            {Taken0, Remaining} = lists:split(N, Sorted),
            Taken = [{T + 1, About} | [{Tx + 1, E} || {Tx, E} <- Taken0]],
            State#state{events = lists:sort(fun sort/2, Taken ++ Remaining)}
    end;
next_state(State, _V, {call, ?MODULE, prune, [Retransmit]}) ->
    Partition = fun({T, _}) -> T < Retransmit end,
    {Keep, Pruned0} = lists:partition(Partition, State#state.events),
    Pruned = lists:foldl(fun({_, E}, Acc) -> [E | Acc] end, State#state.pruned, Pruned0),
    State#state{events = lists:sort(fun sort/2, Keep), pruned = Pruned}.

postcondition(State, {call, ?MODULE, take, []}, Result) ->
    Events = [E || {_, E} <- State#state.events],
    lists:all(fun(M) -> lists:member(M, Events) end, Result);
postcondition(State, {call, ?MODULE, take_target, [Target]}, Result) ->
    Events = [E || {_, E} <- State#state.events],
    AllPresent = lists:all(fun(M) -> lists:member(M, Events) end, Result),
    HasSuspect = lists:any(fun({_, {suspect, _, M, _}}) -> M =:= Target;
                              (_) -> false
                           end, State#state.events),
    SuspectIncluded = case HasSuspect of
                          true ->
                              lists:any(fun({membership, {suspect, _, M, _}}) -> M =:= Target;
                                           (_) -> false
                                        end, Result);
                          false ->
                              true
                      end,
    AllPresent andalso SuspectIncluded;
postcondition(_State, {call, ?MODULE, insert, [_Event]}, _Result) ->
    true;
postcondition(_State, {call, ?MODULE, prune, [_Retransmit]}, _Result) ->
    true.

prop_swim_broadcasts() ->
    ?FORALL(Cmds, commands(?MODULE),
            begin
                start_link(),
                {H, S, R} = run_commands(?MODULE, Cmds),
                stop(),
                ?WHENFAIL(
                   print_results(H, S, R),
                   aggregate(command_names(Cmds), R =:= ok))
            end).

print_results(H, S, R) ->
    io:format("History: ~p~nState: ~p~nResult:~p~n", [H, S, R]).

sort({_, {user, _}}, {_, {membership, _}}) -> false;
sort({_, {membership, _}}, {_, {user, _}}) -> true;
sort(A, B) -> A =< B.

take() ->
    gen_server:call(?MODULE, take, 500).

take_target(Target) ->
    gen_server:call(?MODULE, {take_target, Target}, 500).

insert(Event) ->
    gen_server:call(?MODULE, {insert, Event}, 500).

prune(Retransmits) ->
    gen_server:call(?MODULE, {prune, Retransmits}, 500).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

init([]) ->
    {ok, swim_broadcasts:new(3)}.

handle_call(take, _From, Broadcasts0) ->
    {Take, Broadcasts} = swim_broadcasts:take(Broadcasts0),
    {reply, Take, Broadcasts};
handle_call({take_target, Target}, _From, Broadcasts0) ->
    {Take, Broadcasts} = swim_broadcasts:take(Target, Broadcasts0),
    {reply, Take, Broadcasts};
handle_call({insert, Event}, _From, Broadcasts) ->
    {reply, ok, swim_broadcasts:insert(Event, Broadcasts)};
handle_call({prune, Num}, _From, Broadcasts) ->
    {reply, ok, swim_broadcasts:prune(Num, Broadcasts)}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

prop_retransmit_limit() ->
    ?FORALL({NumMembers, Factor}, {range(1, 1000), range(1, 10)},
            begin
                B = swim_broadcasts:new(Factor),
                Limit = swim_broadcasts:retransmit_limit(NumMembers, B),
                Expected = Factor * ceil(math:log10(NumMembers + 1)),
                Limit > 0 andalso Limit =:= Expected
            end).

prop_invalidation() ->
    ?FORALL({Member, Inc1, Inc2, From},
            {g_member(), g_incarnation(), g_incarnation(), g_member()},
            begin
                B0 = swim_broadcasts:new(3),
                B1 = swim_broadcasts:insert({membership, {alive, Inc1, Member}}, B0),
                B2 = swim_broadcasts:insert({membership, {suspect, Inc2, Member, From}}, B1),
                {Taken, _} = swim_broadcasts:take(B2),
                MemberEvents = [E || {membership, E} <- Taken,
                                     event_member(E) =:= Member],
                length(MemberEvents) =< 1 andalso
                case MemberEvents of
                    [{suspect, Inc2, Member, From}] -> true;
                    [] -> true;
                    _ -> false
                end
            end).

event_member({alive, _, M}) -> M;
event_member({suspect, _, M, _}) -> M;
event_member({faulty, _, M, _}) -> M.
