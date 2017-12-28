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

-module(swim_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ListenIP        = application:get_env(swim, ip, {127,0,0,1}),
    ListenPort      = application:get_env(swim, port, 5000),
    AckTimeout      = application:get_env(swim, ack_timeout, 100),
    NackTimeout     = application:get_env(swim, nack_timeout, floor(AckTimeout * 0.8)),
    ProbeTimeout    = application:get_env(swim, probe_timeout, 500),
    ProtocolPeriod  = application:get_env(swim, protocol_period, 1000),
    NumProxies      = application:get_env(swim, num_proxies, 3),
    SuspicionFactor = application:get_env(swim, suspicion_factor, 3),
    AwarenessCount  = application:get_env(swim, awareness_count, 8),
    Alpha           = application:get_env(swim, alpha, 5),
    Beta            = application:get_env(swim, beta, 6),
    Retransmits     = application:get_env(swim, retransmit_factor, 3),
    LocalMember     = {ListenIP, ListenPort},
    Membership      = swim_membership:new(LocalMember, Alpha, Beta, ProbeTimeout, SuspicionFactor),
    Broadcasts      = swim_broadcasts:new(Retransmits),
    Awareness       = swim_awareness:new(AwarenessCount),
    StateOpts = #{
      protocol_period  => ProtocolPeriod,
      probe_timeout    => ProbeTimeout,
      ack_timeout      => AckTimeout,
      num_proxies      => NumProxies
     },
    State = #{id => state,
              start => {swim_state, start_link, [Membership, Broadcasts, Awareness, StateOpts]}},
    Keyring = swim_keyring:new(get_key()),
    Failure = #{id => failure,
                start => {swim_failure, start_link,
                          [LocalMember, Keyring, AckTimeout, NackTimeout]}},
    PushPull = #{id => pushpull,
                 start => {swim_pushpull_sup, start_link, [ListenIP, ListenPort]}},
    Metrics = #{id => metrics,
                start => {swim_metrics, start_link, []}},
    Flags = #{strategy => rest_for_one,
              intensity => 5,
              period => 900
             },
    {ok, {Flags, [State, Failure, PushPull, Metrics]}}.

read_key_file({ok, KeyFile}) ->
    {ok, EncodedKey} = file:read_file(KeyFile),
    [base64:decode(EncodedKey)];
read_key_file(undefined) ->
    [crypto:strong_rand_bytes(32)].

get_key() ->
    case application:get_env(swim, key) of
        {ok, Base64Key} ->
            [base64:decode(Base64Key)];
        undefined ->
            read_key_file(application:get_env(swim, keyfile))
    end.
