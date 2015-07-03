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

%%% @version {@version}

%%% @doc This is the main swim module.
-module(swim).

-export([start_gossip/4,
         stop_gossip/1,
         publish/2,
         subscribe/2,
         unsubscribe/2,
         members/1,
	 rotate_keys/2]).

-include("swim.hrl").

-export_type([member/0, member_status/0, incarnation/0, user_event/0,
	      membership_event/0, swim_event/0, event_category/0]).

%% @doc Starts a gossip peer.
-spec start_gossip(atom(), inet:ip_address(),
		   inet:port_number(), [swim_gossip:gossip_opt()])
		  -> {ok, pid()}.
start_gossip(Name, ListenIp, ListenPort, Opts) ->
    swim_sup:start_gossip(Name, ListenIp, ListenPort, Opts).

%% @doc Stops a running gossip peer.
-spec stop_gossip(atom()) -> ok.
stop_gossip(Name) ->
    swim_sup:stop_gossip(Name).

%% @doc Returns the known members for a given gossip peer.
-spec members(atom()) -> [member()].
members(Name) ->
    swim_gossip:members(Name).

%% @doc Publishes a term to the rest of the gossip peers.
-spec publish(atom(), term()) -> ok.
publish(Name, Event) ->
    swim_gossip:gossip(Name, Event).

%% @doc Subscribes to events on a given gossip group.
%%
%% A subscribing process will receive messages based on the event-category.
%% @end
-spec subscribe(atom(), event_category()) -> ok.
subscribe(Name, EventCategory) ->
    swim_gossip_events:subscribe(Name, EventCategory, self()).

%% @doc Unsubscribe from an event category.
-spec unsubscribe(atom(), event_category()) -> ok.
unsubscribe(Name, EventCategory) ->
    swim_gossip_events:unsubscribe(Name, EventCategory, self()).

%% @doc Rotate the encryption keys for a local gossip peer.
-spec rotate_keys(atom(), key()) -> ok.
rotate_keys(Name, Key) ->
    swim_gossip:rotate_keys(Name, Key).
