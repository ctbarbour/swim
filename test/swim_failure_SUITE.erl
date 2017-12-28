-module(swim_failure_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

-export([ping/1]).
-export([ping_req/1]).

all() ->
    [{group, with_client}].

groups() ->
    [{with_client, [shuffle, sequence], [ping, ping_req]}].

local_member() ->
    {{127,0,0,1}, 9200}.

remote_member() ->
    {{127,0,0,1}, 9000}.

init_per_suite(Config) ->
    error_logger:tty(false),
    Key = crypto:strong_rand_bytes(32),
    RemoteMember = remote_member(),
    ok = application:set_env(swim, port, element(2, RemoteMember)),
    ok = application:set_env(swim, key, base64:encode(Key)),
    ok = application:start(swim),
    [{local_member, local_member()}, {remote_member, RemoteMember}, {key, Key} | Config].

end_per_suite(_Config) ->
    ok = application:stop(swim),
    error_logger:tty(true),
    ok.

init_per_group(with_client, Config) ->
    {ok, Client} = swim_test_client:start(?config(local_member, Config),
                                          ?config(key, Config)),
    [{client, Client} | Config].

end_per_group(with_client, Config) ->
    Client = ?config(client, Config),
    ok = swim_test_client:stop(Client),
    Config.

ping(Config) ->
    Target = ?config(remote_member, Config),
    {ack, 1, Target} = call({ping, 1, Target}, Config),
    ok.

ping_req(Config) ->
    Target = ?config(remote_member, Config),
    Terminal = ?config(local_member, Config),
    {ack, 2, Terminal} = call({ping_req, 2, Target, Terminal}, Config),
    ok.

call(Msg, Config) ->
    swim_test_client:call(?config(client, Config), Msg).
