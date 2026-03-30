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

-export([start_link/0, start_link/2]).
-export([init/1]).

start_link() ->
    start_link(default, #{}).

start_link(Name, Config) ->
    supervisor:start_link({local, swim_name:proc_name(Name, sup)}, ?MODULE, [Name, Config]).

init([Name, Config]) ->
    ListenIP        = get_config(ip, Config, {127,0,0,1}),
    ListenPort      = get_config(port, Config, 5000),
    AckTimeout      = get_config(ack_timeout, Config, 100),
    ProbeTimeout    = get_config(probe_timeout, Config, 500),
    NackTimeout     = get_config(nack_timeout, Config, floor(ProbeTimeout * 0.8)),
    ProtocolPeriod  = get_config(protocol_period, Config, 1000),
    NumProxies      = get_config(num_proxies, Config, 3),
    SuspicionFactor = get_config(suspicion_factor, Config, 3),
    AwarenessCount  = get_config(awareness_count, Config, 8),
    Alpha           = get_config(alpha, Config, 5),
    Beta            = get_config(beta, Config, 6),
    Retransmits     = get_config(retransmit_factor, Config, 3),
    MaxMessageSize  = get_config(max_message_size, Config, 452),
    LocalMember     = {ListenIP, ListenPort},
    Membership      = swim_membership:new(LocalMember, Alpha, Beta, ProbeTimeout, SuspicionFactor),
    Broadcasts      = swim_broadcasts:new(Retransmits, MaxMessageSize),
    Awareness       = swim_awareness:new(AwarenessCount),
    Keyring         = swim_keyring:new(get_key(Config)),
    StateOpts = #{
      protocol_period  => ProtocolPeriod,
      probe_timeout    => ProbeTimeout,
      ack_timeout      => AckTimeout,
      nack_timeout     => NackTimeout,
      num_proxies      => NumProxies
     },
    Subscriptions = #{id => {Name, subscriptions},
                      start => {swim_subscriptions, start_link, [Name]}},
    Metrics = #{id => {Name, metrics},
                start => {swim_metrics, start_link, [Name]}},
    State = #{id => {Name, state},
              start => {swim_state, start_link,
                        [Name, LocalMember, Keyring, Membership, Broadcasts, Awareness, StateOpts]}},
    PushPull = #{id => {Name, pushpull},
                 start => {swim_pushpull_sup, start_link, [Name, ListenIP, ListenPort]}},
    Flags = #{strategy => rest_for_one,
              intensity => 5,
              period => 900
             },
    {ok, {Flags, [Subscriptions, Metrics, State, PushPull]}}.

get_config(Key, Config, Default) ->
    case maps:find(Key, Config) of
        {ok, Value} -> Value;
        error -> application:get_env(swim, Key, Default)
    end.

get_key(Config) ->
    case maps:find(key, Config) of
        {ok, Base64Key} ->
            [base64:decode(Base64Key)];
        error ->
            case application:get_env(swim, key) of
                {ok, Base64Key} ->
                    [base64:decode(Base64Key)];
                undefined ->
                    read_key_file(application:get_env(swim, keyfile))
            end
    end.

read_key_file({ok, KeyFile}) ->
    {ok, EncodedKey} = file:read_file(KeyFile),
    [base64:decode(EncodedKey)];
read_key_file(undefined) ->
    [crypto:strong_rand_bytes(32)].
