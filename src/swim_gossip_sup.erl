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

%% @private
-module(swim_gossip_sup).
-behavior(supervisor).

-export([start_link/3]).
-export([init/1]).

start_link(Name, LocalMember, Opts) ->
    supervisor:start_link(?MODULE, [Name, LocalMember, Opts]).

init([Name, LocalMember, Opts]) ->
    Handler = {swim_gossip_events, Name},
    EventHandler = {{swim_gossip_events, Name},
                   {swim_event_handler_guard, start_link,
                    [swim_gossip_events, Handler, [Name]]},
                   transient, 5000, worker, [swim_event_handler_guard]},
    Gossip = {{swim_gossip_v2, Name},
	      {swim_gossip_v2, start_link, [Name, LocalMember, Opts]},
	      transient, 5000, worker, [swim_gossip_v2]},
    {ok, {{one_for_all, 10, 3600}, [EventHandler, Gossip]}}.
