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
    {ok, ListenIP} = application:get_env(swim, ip),
    {ok, ListenPort} = application:get_env(swim, port),
    Seeds = application:get_env(swim, seeds, []),
    Keys = read_key_file(application:get_env(swim, keyfile)),
    State = #{id => state,
              start => {swim_state, start_link, [{seeds, Seeds}]}},
    Network = #{id => network,
                start => {swim_transport, start_link,
                          [ListenIP, ListenPort, Keys]}},
    Flags = #{strategy => rest_for_one,
              intensity => 5,
              period => 900
             },
    {ok, {Flags, [State, Network]}}.

read_key_file({ok, KeyFile}) ->
    {ok, EncodedKey} = file:read_file(KeyFile),
    [base64:decode(EncodedKey)];
read_key_file(undefined) ->
    [].
