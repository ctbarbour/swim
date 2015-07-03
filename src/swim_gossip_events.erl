%%% ----------------------------------------------------------------------------
%%% Copyright (c) 2015. All Rights Reserved.
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

%%% @copyright 2015
%%% @version {@version}

-module(swim_gossip_events).
-behavior(gen_event).

-export([start_link/0, subscribe/3, unsubscribe/3,
         membership/2, user/2, notify/3]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {
	  subscriptions   :: [subscription()],
	  name            :: atom()
	 }).

-record(subscription, {
	  pid               :: pid(),
	  event_category    :: event_category(),
	  mref              :: reference()
	 }).

-type subscription()   :: #subscription{}.
-type event_category() :: membership | user | system.

start_link() ->
    gen_event:start_link({local, ?MODULE}).

subscribe(Name, EventCategory, Pid) ->
    gen_event:call(?MODULE, {?MODULE, Name}, {subscribe, EventCategory, Pid}).

unsubscribe(Name, EventCategory, Pid) ->
    gen_event:call(?MODULE, {?MODULE, Name}, {unsubscribe, EventCategory, Pid}).

membership(Name, Event) ->
    notify(Name, membership, Event).

user(Name, Event) ->
    notify(Name, user, Event).

notify(Name, EventCategory, Event) ->
    gen_event:notify(?MODULE, {Name, EventCategory, Event}).

init([Name]) ->
    {ok, #state{name=Name, subscriptions=[]}}.

handle_event({Name, EventCategory, Event}, #state{name=Name} = State) ->
    #state{subscriptions=Subscriptions} = State,
    Data = {swim, {Name, {EventCategory, Event}}},
    _ = [Pid ! Data || #subscription{pid=Pid, event_category=EC} <- Subscriptions,
		       EC == EventCategory],
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

handle_call({subscribe, EventCategory, Pid}, State) ->
    #state{subscriptions=Subscriptions} = State,
    MRef = erlang:monitor(process, Pid),
    Subscription = #subscription{pid=Pid, event_category=EventCategory, mref=MRef},
    {ok, ok, State#state{subscriptions=[Subscription|Subscriptions]}};
handle_call({unsubscribe, _EventCategory, Pid}, State) ->
    #state{subscriptions=Subscriptions} = State,
    case lists:keytake(Pid, #subscription.pid, Subscriptions) of
	{value, #subscription{mref=MRef}, Rest} ->
	    _ = erlang:demonitor(MRef),
	    {ok, ok, State#state{subscriptions=Rest}};
	false ->
	    {ok, ok, State}
    end;
handle_call(_Msg, State) ->
    {ok, ok, State}.

handle_info({'DOWN', MRef, process, Pid, _Reason}, State) ->
    #state{subscriptions=Subscriptions} = State,
    case lists:keytake(MRef, #subscription.mref, Subscriptions) of
	{value, #subscription{pid=Pid}, Rest} ->
	    {ok, State#state{subscriptions=Rest}};
	false ->
	    {ok, State}
    end;
handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
