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

%%% @copyright 2015-2017
%%% @version {@version}

-module(swim_subscriptions).
-behavior(gen_event).

-export([start_link/0, start_link/1]).

-export([subscribe/2, subscribe/3]).
-export([unsubscribe/2, unsubscribe/3]).
-export([publish/1, publish/2]).

-export([init/1]).
-export([handle_event/2]).
-export([handle_call/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
          pid            :: pid(),
          event_category :: user | membership,
          mref           :: reference()
         }).

start_link() ->
    start_link(default).

start_link(Name) ->
    gen_event:start_link({local, swim_name:proc_name(Name, subscriptions)}).

subscribe(EventCategory, Pid) ->
    subscribe(swim_name:proc_name(default, subscriptions), EventCategory, Pid).

subscribe(ServerRef, EventCategory, Pid) ->
    gen_event:add_handler(ServerRef, ?MODULE, [EventCategory, Pid]).

unsubscribe(EventCategory, Pid) ->
    unsubscribe(swim_name:proc_name(default, subscriptions), EventCategory, Pid).

unsubscribe(ServerRef, EventCategory, Pid) ->
    gen_event:delete_handler(ServerRef, ?MODULE, [EventCategory, Pid]).

publish(Events) ->
    publish(swim_name:proc_name(default, subscriptions), Events).

publish(ServerRef, Events) when is_list(Events) ->
    [publish(ServerRef, Event) || Event <- Events],
    ok;
publish(ServerRef, {membership, {alive, _, M}}) ->
    gen_event:notify(ServerRef, {membership, {alive, M}});
publish(ServerRef, {membership, {faulty, _, M, T}}) ->
    gen_event:notify(ServerRef, {membership, {faulty, M, T}});
publish(ServerRef, {user, Event}) ->
    gen_event:notify(ServerRef, {user, Event});
publish(_ServerRef, _) ->
    ok.

init([EventCategory, Pid]) ->
    MRef = erlang:monitor(process, Pid),
    {ok, #state{pid = Pid, event_category = EventCategory, mref = MRef}}.

handle_event({EventCategory, _Event} = Data, State)
  when State#state.event_category =:= EventCategory ->
    State#state.pid ! {swim, Data},
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Msg, State) ->
    {ok, ok, State}.

handle_info({'DOWN', MRef, process, Pid, _Reason}, State)
  when State#state.mref =:= MRef andalso State#state.pid =:= Pid ->
    remove_handler;
handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
