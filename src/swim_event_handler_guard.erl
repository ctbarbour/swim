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

%% @private
-module(swim_event_handler_guard).
-behavior(gen_server).

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {
	  handler_mod :: module(),
	  handler     :: module()
	 }).

start_link(EventMgrRef, Handler, Args) ->
    gen_server:start_link(?MODULE, [EventMgrRef, Handler, Args], []).

init([HandlerMod, Handler, Args]) ->
    ok = gen_event:add_sup_handler(HandlerMod, Handler, Args),
    {ok, #state{handler_mod=HandlerMod, handler=Handler}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({gen_event_EXIT, Handler, shutdown}, #state{handler=Handler} = State) ->
    {stop, normal, State};

handle_info({gen_event_EXIT, Handler, normal}, #state{handler=Handler} = State) ->
    {stop, normal, State};

handle_info({gen_event_EXIT, Handler, Reason}, #state{handler=Handler} = State) ->
    {stop, {gen_event_EXIT, Reason}, State};

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
