-module(swim_transport_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0, init_per_group/2, end_per_group/2,
	 init_per_testcase/2, end_per_testcase/2]).
-export([send_ack/1, send_ping/1, send_ping_req/1]).

-define(match_message(Msg), begin
				receive
				    Msg ->
					ok
				after
				    1000 ->
					{error, timeout}
				end
			    end).

all() ->
    [{group, send_and_receive}].

groups() ->
    [{send_and_receive, [shuffle, sequence],
      [send_ping, send_ping_req, send_ack]}].

keys() ->
    [crypto:rand_bytes(32)].

local_ipv4() ->
    {ok, Addrs} = inet:getifaddrs(),
    hd([Addr || {_, Opts} <- Addrs,
		{addr, Addr} <- Opts,
		size(Addr) == 4,
		Addr =/= {127,0,0,1}]).

init_per_group(send_and_receive, Config) ->
    Keys = keys(),
    LocalIp = local_ipv4(),
    [{keys, Keys}, {ip, LocalIp} | Config];
init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_, Config) ->
    {ok, Alice} = swim_transport:start_link(?config(ip, Config), 1337,
					    ?config(keys, Config)),
    {ok, Bob} = swim_transport:start_link(?config(ip, Config), 1338,
					  ?config(keys, Config)),
    [{alice, Alice}, {bob, Bob} | Config].

end_per_testcase(_, _Config) ->
    ok.

send_ack(Config) ->
    Bob = ?config(bob, Config),
    Msg = swim_messages:encode_ack(1, {?config(ip, Config), 1338}, []),
    ok = swim_transport:send(Bob, ?config(ip, Config), 1337, Msg),
    ok = ?match_message({ack, 1, _Responder, []}).

send_ping(Config) ->
    Alice = ?config(alice, Config),
    Msg = swim_messages:encode_ping(1, []),
    ok = swim_transport:send(Alice, ?config(ip, Config), 1338, Msg),
    ok = ?match_message({ping, 1, []}).

send_ping_req(Config) ->
    Alice = ?config(alice, Config),
    Msg = swim_messages:encode_ping_req(1, {?config(ip, Config), 1337}),
    ok = swim_transport:send(Alice, ?config(ip, Config), 1338, Msg),
    ok = ?match_message({ping_req, 1, _Responder}).
