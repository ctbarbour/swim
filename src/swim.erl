%%% ----------------------------------------------------------------------------
%%% Copyright (c) 2015-2017. All Rights Reserved.
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

-module(swim).

%% Named instance API
-export([start/2]).
-export([stop/1]).
-export([join/2]).
-export([members/1]).
-export([myself/1]).
-export([publish/2]).
-export([subscribe/2]).
-export([unsubscribe/2]).

%% Default instance API (backwards compatibility)
-export([join/1]).
-export([members/0]).
-export([myself/0]).
-export([publish/1]).
-export([subscribe/1]).
-export([unsubscribe/1]).

-type member()           :: {inet:ip_address(), inet:port_number()}.
-type incarnation()      :: non_neg_integer().
-type user_event()       :: binary().
-type membership_event() :: alive_event() | suspect_event() | faulty_event().
-type suspect_event()    :: {suspect, incarnation(), member(), member()}.
-type alive_event()      :: {alive, incarnation(), member()}.
-type faulty_event()     :: {faulty, incarnation(), member(), member()}.
-type swim_event()       :: {user, user_event()} | {membership, membership_event()}.

-export_type([swim_event/0]).
-export_type([member/0]).
-export_type([incarnation/0]).
-export_type([user_event/0]).
-export_type([membership_event/0]).

%%% ===================================================================
%%% Named instance API
%%% ===================================================================

-spec start(atom(), map()) -> {ok, pid()} | {error, term()}.

start(Name, Config) ->
    swim_sup:start_link(Name, Config).

-spec stop(atom()) -> ok.

stop(Name) ->
    case whereis(swim_name:proc_name(Name, sup)) of
        undefined -> ok;
        Pid ->
            Ref = erlang:monitor(process, Pid),
            exit(Pid, shutdown),
            receive
                {'DOWN', Ref, process, Pid, _} -> ok
            end
    end.

join(Name, Seed) when is_atom(Name) ->
    swim_pushpull:join(Name, Seed, #{}).

members(Name) when is_atom(Name) ->
    [M || {M, _S, _I} <- swim_state:members(swim_name:proc_name(Name, state))].

myself(Name) when is_atom(Name) ->
    swim_state:local_member(swim_name:proc_name(Name, state)).

publish(Name, Msg) when is_atom(Name), is_binary(Msg) ->
    swim_state:publish(swim_name:proc_name(Name, state), Msg).

subscribe(Name, metrics) when is_atom(Name) ->
    swim_metrics:subscribe(swim_name:proc_name(Name, metrics), self());
subscribe(Name, EventCategory) when is_atom(Name) ->
    swim_subscriptions:subscribe(swim_name:proc_name(Name, subscriptions), EventCategory, self()).

unsubscribe(Name, metrics) when is_atom(Name) ->
    swim_metrics:unsubscribe(swim_name:proc_name(Name, metrics), self());
unsubscribe(Name, EventCategory) when is_atom(Name) ->
    swim_subscriptions:unsubscribe(swim_name:proc_name(Name, subscriptions), EventCategory, self()).

%%% ===================================================================
%%% Default instance API (backwards compatibility)
%%% ===================================================================

join(Seed) ->
    join(default, Seed).

members() ->
    members(default).

myself() ->
    myself(default).

publish(Msg) when is_binary(Msg) ->
    publish(default, Msg).

subscribe(EventCategory) ->
    subscribe(default, EventCategory).

unsubscribe(EventCategory) ->
    unsubscribe(default, EventCategory).
