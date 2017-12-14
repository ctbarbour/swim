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

-export([subscribe/3]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {
          subscriptions = [] :: [subscription()]
         }).

-record(subscription, {
          pid :: pid(),
          event_category :: user | membership,
          mref :: reference()
         }).

-type subscription() :: #subscription{}.

subscribe(EventMgrPid, EventCategory, Pid) ->
    gen_event:call(EventMgrPid, ?MODULE, {subscribe, EventCategory, Pid}).

init([Pid]) ->
    MRef = erlang:monitor(process, Pid),
    M = #subscription{pid=Pid, event_category=membership, mref=MRef},
    {ok, #state{subscriptions=[M]}};
init([]) ->
    {ok, #state{subscriptions=[]}}.

handle_event({EventCategory, Event}, State) ->
    #state{subscriptions=Subscriptions} = State,
    Data = {swim, {EventCategory, Event}},
    _ = [Pid ! Data ||
            #subscription{pid=Pid, event_category=EC} <- Subscriptions,
            EC == EventCategory],
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

handle_call({subscribe, EventCategory, Pid}, State) ->
    #state{subscriptions=Subscriptions} = State,
    case lists:keyfind(Pid, #subscription.pid, Subscriptions) of
        false ->
            MRef = erlang:monitor(process, Pid),
            Subscription = #subscription{pid=Pid,
                                         event_category=EventCategory,
                                         mref=MRef},
            {ok, ok, State#state{subscriptions=[Subscription | Subscriptions]}};
        _ ->
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
