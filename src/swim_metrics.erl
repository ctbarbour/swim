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

-module(swim_metrics).
-behavior(gen_event).

-export([start_link/0]).
-export([notify/1]).
-export([subscribe/1]).
-export([unsubscribe/1]).

-export([init/1]).
-export([handle_event/2]).
-export([handle_call/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

start_link() ->
    gen_event:start_link({local, ?MODULE}).

subscribe(Pid) ->
    gen_event:add_handler(?MODULE, ?MODULE, [Pid]).

unsubscribe(Pid) ->
    gen_event:delete_handler(?MODULE, ?MODULE, [Pid]).

notify(Event) ->
    gen_event:notify(?MODULE, Event).

init([Subscriber]) ->
    erlang:monitor(process, Subscriber),
    {ok, Subscriber}.

handle_event(Event, Subscriber) ->
    Subscriber ! Event,
    {ok, Subscriber}.

handle_call(Request, State) ->
    {ok, Request, State}.

handle_info({'DOWN', _MRef, process, Subscriber, _Reason}, Subscriber) ->
    remove_handler;
handle_info(_Info, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
