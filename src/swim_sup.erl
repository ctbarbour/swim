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
-module(swim_sup).
-behaviour(supervisor).

-export([start_link/0, start_gossip/4, stop_gossip/1]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_gossip(atom(), inet:ip_address(), inet:port_number(), list())
		  -> {ok, pid()} | {error, term()}.
start_gossip(Name, ListenIp, ListenPort, Opts) ->
    Spec = {{swim_gossip_sup, Name},
	    {swim_gossip_sup, start_link, [Name, ListenIp, ListenPort, Opts]},
	    transient, 5000, supervisor, [swim_gossip_sup]},
    supervisor:start_child(?MODULE, Spec).

-spec stop_gossip(atom()) -> ok.
stop_gossip(Name) ->
    Id = {swim_gossip_sup, Name},
    ok = supervisor:terminate_child(?MODULE, Id),
    ok = supervisor:delete_child(?MODULE, Id),
    ok.

init([]) ->
    Events = {swim_gossip_events, {swim_gossip_events, start_link, []},
	      permanent, 5000, worker, [swim_gossip_events]},
    {ok, {{one_for_one, 10, 3600}, [Events]} }.
