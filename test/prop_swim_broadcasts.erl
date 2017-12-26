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

-import(swim_generators, [swim_event/0]).

-record(state, {
          events = [] :: {non_neg_integer(), swim:swim_event()},
          pruned = [] :: swim:membership_event()
         }).

g_retransmits() ->
    ?LET({NumNumbers, RetransmitFactor}, {range(1, 10), exactly(3)},
         swim_broadcasts:retransmit_limit(NumNumbers, RetransmitFactor)).

initial_state() ->
    #state{events = [], pruned = []}.

command(_State) ->
    frequency([
               {1, {call, ?MODULE, insert, [swim_event()]}},
               {1, {call, ?MODULE, take, [range(1,5)]}},
               {1, {call, ?MODULE, prune, [g_retransmits()]}}
              ]).

precondition(#state{events = []}, {call, ?MODULE, take, _}) ->
    false;
precondition(#state{events = []}, {call, ?MODULE, prune, _}) ->
    false;
precondition(_State, _Call) ->
    true.

next_state(State, _V, {call, ?MODULE, insert, [Event]}) ->
    State#state{events = lists:sort(fun sort/2, [{0, Event} | State#state.events])};
next_state(State, _V, {call, ?MODULE, take, [Max]}) ->
    N = min(length(State#state.events), Max),
    {Taken0, Rest} = lists:split(N, lists:sort(fun sort/2, State#state.events)),
    Taken = [{T + 1, E} || {T, E} <- Taken0],
    State#state{events = lists:sort(fun sort/2, Taken ++ Rest)};
next_state(State, _V, {call, ?MODULE, prune, [Retransmit]}) ->
    Partition = fun({T, _}) -> T < Retransmit end,
    {Keep, Pruned0} = lists:partition(Partition, State#state.events),
    Pruned = lists:foldl(fun({_, E}, Acc) -> [E | Acc] end, State#state.pruned, Pruned0),
    State#state{events = lists:sort(fun sort/2, Keep), pruned = Pruned}.

postcondition(State, {call, ?MODULE, take, [_Max]}, Result) ->
    Events = [E || {_, E} <- State#state.events],
    lists:all(fun(M) -> lists:member(M, Events) end, Result);
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

take(Take) ->
    gen_server:call(?MODULE, {take, Take}, 500).

insert(Event) ->
    gen_server:call(?MODULE, {insert, Event}, 500).

prune(Retransmits) ->
    gen_server:call(?MODULE, {prune, Retransmits}, 500).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

init([]) ->
    {ok, swim_broadcasts:new()}.

handle_call({take, Max}, _From, Broadcasts0) ->
    {Take, Broadcasts} = swim_broadcasts:take(Max, Broadcasts0),
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
