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

-export([join/2]).
-export([members/0]).
-export([myself/0]).
-export([subscribe/0]).
-export([unsubscribe/0]).
-export([setup/0]).

-type member()           :: {inet:ip_address(), inet:port_number()}.
-type incarnation()      :: non_neg_integer().
-type user_event()       :: any().
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

join(Seed, LocalMember) ->
    swim_pushpull:join(Seed, LocalMember, #{}).

members() ->
    [M || {_S, _I, M} <- swim_state:members()].

myself() ->
    swim_state:local_member().

subscribe() ->
    swim_metrics:subscribe(self()).

unsubscribe() ->
    swim_metrics:unsubscribe(self()).

setup() ->
    Key = base64:encode(crypto:strong_rand_bytes(32)),
    BasePort = 5000,
    Ms = lists:zip(
           [{{127,0,0,1}, P} || P <- lists:seq(BasePort, length(nodes()) + BasePort)],
           [node() | nodes()]),
    [rpc:call(Node, application, set_env, [swim, port, Port]) || {{_, Port}, Node} <- Ms],
    [rpc:call(Node, application, set_env, [swim, key, Key]) || Node <- [node() | nodes()]],
    [rpc:call(Node, application, start, [swim]) || Node <- [node() | nodes()]].
