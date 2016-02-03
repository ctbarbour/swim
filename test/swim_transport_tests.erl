-module(swim_transport_tests).

-include_lib("eunit/include/eunit.hrl").
-define(match_message(Msg), begin
				 receive
				     Msg ->
					 true
				 after
				     1000 ->
					 false
				 end
			    end).

local_ipv4() ->
    {ok, Addrs} = inet:getifaddrs(),
    hd([Addr || {_, Opts} <- Addrs,
		{addr, Addr} <- Opts,
		size(Addr) == 4,
		Addr =/= {127,0,0,1}]).
start() ->
    Keys = [crypto:rand_bytes(32)],
    LocalIp = local_ipv4(),
    {ok, Alice} = swim_transport:start_link(LocalIp, 4678, Keys),
    {ok, Bob} = swim_transport:start_link(LocalIp, 4679, Keys),
    #{alice => {Alice, 4678}, bob => {Bob, 4679}, local_ip => LocalIp}.

stop(Config) ->
    #{alice := {Alice, _}, bob := {Bob, _}} = Config,
    [swim_transport:close(P) || P <- [Alice, Bob]],
    ok.

swim_transport_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun(Config) ->
	     [send_and_receive_ping(Config),
	      send_and_receive_ack(Config),
	      send_and_receive_ping_req(Config)]
     end}.

send_and_receive_ping(Config) ->
    #{alice := {Alice, _}, bob := {_, Port}, local_ip := LocalIp} = Config,
    Msg = swim_messages:encode_ping(1, []),
    ok = swim_transport:send(Alice, LocalIp, Port, Msg),
    Result = ?match_message({{ping, 1, []}, _}),
    ?_assert(Result).

send_and_receive_ack(Config) ->
    #{alice := {Alice, _}, bob := {_, Port}, local_ip := LocalIp} = Config,
    Msg = swim_messages:encode_ack(1, {LocalIp, Port}, []),
    ok = swim_transport:send(Alice, LocalIp, Port, Msg),
    Result = ?match_message({{ack, 1, _Responder, []}, _}),
    ?_assert(Result).

send_and_receive_ping_req(Config) ->
    #{alice := {Alice, _}, bob := {_, Port}, local_ip := LocalIp} = Config,
    Msg = swim_messages:encode_ping_req(1, {LocalIp, Port}),
    ok = swim_transport:send(Alice, LocalIp, Port, Msg),
    Result = ?match_message({{ping_req, 1, _Responser}, _}),
    ?_assert(Result).
