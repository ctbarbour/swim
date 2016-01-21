-module(swim_gossip_v2_tests).

-include_lib("eunit/include/eunit.hrl").

start() ->
    _ = application:ensure_all_started(damocles),
    _ = damocles_lib:teardown_all_local_interface(),
    Keys = [crypto:rand_bytes(32)],
    Interfaces = [{"10.10.10.10", 5555}, {"10.10.10.11", 5556},
		  {"10.10.10.12", 5557}, {"10.10.10.13", 5558},
		  {"10.10.10.14", 5559}],
    start_members(Keys, Interfaces).

stop(Pids) ->
    _ = [swim_gossip_v2:stop(P) || P <- Pids],
    _ = damocles_lib:teardown_all_local_interface(),
    _ = damocles_lib:teardown_traffic_control(),
    _ = damocles:stop(),
    ok.

start_members(Keys, [{Address, Port} | Rest]) ->
    _ = damocles:add_interface(Address),
    {ok, Ip} = inet:parse_ipv4_address(Address),
    {ok, Gossip} = swim_gossip_v2:start_link({Ip, Port}, [{keys, Keys}]),
    start_other_members([{Ip, Port}], Keys, Rest, [Gossip]).

start_other_members(_Seeds, _Keys, [], Acc) ->
    Acc;
start_other_members(Seeds, Keys, [{Address, Port} | Rest], Acc) ->
    _ = damocles:add_interface(Address),
    {ok, Ip} = inet:parse_ipv4_address(Address),
    {ok, Gossip} = swim_gossip_v2:start_link({Ip, Port},
					     [{keys, Keys},
					      {seeds, Seeds}]),
    start_other_members(Seeds, Keys, Rest, [Gossip | Acc]).

start_link_test_() ->
    {timeout, 60,
     {setup,
      fun start/0,
      fun stop/1,
      fun(Pids) ->
	      _ = damocles:isolate_between_interfaces(["10.10.10.11", "10.10.10.11"],
						      ["10.10.10.13", "10.10.10.14"]),
	      _ = damocles:packet_loss_global(0.40),
	      _ = damocles:delay_global(100),
	      ok = timer:sleep(10000),
	      [First | Rest] = lists:map(
				 fun(Pid) ->
					 Ms = swim_gossip_v2:members(Pid),
					 L = swim_gossip_v2:local_member(Pid),
					 lists:sort([L | lists:map(
							   fun({M, _, _}) ->
								   M
							   end, Ms)])
				 end, Pids),
	      ?_assert(lists:all(fun(Ms) ->
					 Ms == First
				 end, Rest))
      end
     }}.
