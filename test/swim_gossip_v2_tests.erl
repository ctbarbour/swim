-module(swim_gossip_v2_tests).

-include_lib("eunit/include/eunit.hrl").

start() ->
    dbg:start(),
    dbg:tracer(),
    dbg:tpl(swim_gossip_v2, handle_info, [{'_', [], [{return_trace}]}]),
    dbg:tpl(swim_gossip_v2, send_ping, [{'_', [], [{return_trace}]}]),
    dbg:tpl(swim_gossip_v2, send_ack, [{'_', [], [{return_trace}]}]),
    dbg:p(all, c),
    _ = application:ensure_all_started(damocles),
    _ = damocles_lib:teardown_all_local_interface(),
    Keys = [crypto:rand_bytes(32)],
    Interfaces = [{"10.10.10.10", 5555}, {"10.10.10.11", 5556},
		  {"10.10.10.12", 5557}],
    [Keys, start_members(Keys, Interfaces)].

stop(_) ->
    _ = damocles_lib:teardown_all_local_interface(),
    _ = damocles_lib:teardown_traffic_control(),
    _ = damocles:stop(),
    dbg:ctp(swim_gossip_v2),
    ok.

start_members(Keys, [{Address, Port} | Rest]) ->
    _ = damocles:add_interface(Address),
    {ok, Ip} = inet:parse_ipv4_address(Address),
    {ok, Membership} = swim_membership_v2:start_link({Ip, Port}, []),
    {ok, Gossip} = swim_gossip_v2:start_link(Membership, [{ip, Ip}, {port, Port}, {keys, Keys}], []),
    start_other_members([{Ip, Port}], Keys, Rest, [{Membership, Gossip}]).

start_other_members(_Seeds, _Keys, [], Acc) ->
    Acc;
start_other_members(Seeds, Keys, [{Address, Port} | Rest], Acc) ->
    _ = damocles:add_interface(Address),
    {ok, Ip} = inet:parse_ipv4_address(Address),
    {ok, Membership} = swim_membership_v2:start_link({Ip, Port}, [{seeds, Seeds}]),
    {ok, Gossip} = swim_gossip_v2:start_link(Membership, [{ip, Ip}, {port, Port},
							  {keys, Keys}], []),
    start_other_members(Seeds, Keys, Rest, [{Membership, Gossip} | Acc]).

start_link_test_() ->
    {timeout, 60,
     {setup,
      fun start/0,
      fun stop/1,
      fun([_Keys, [{Membership, _} | MemberPids]]) ->
	      _ = damocles:isolate_between_interfaces(["10.10.10.11"], ["10.10.10.12"]),
	      _ = damocles:packet_loss_global(0.40),
	      receive
		  Msg ->
		      ?debugFmt("Received: ~p", [Msg]),
		      ok
	      after
		  10000 ->
		      ok
	      end,
	      Members = swim_membership_v2:members(Membership),
	      ?debugFmt("Members: ~p", [Members]),
	      ?_assert(lists:all(fun({M, _}) ->

					 Ms = swim_membership_v2:members(M),
					 ?debugFmt("Members: ~p", [Ms]),
					 Ms == Members
				 end, MemberPids))
      end
     }}.
