-module(swim_tests).

-include_lib("eunit/include/eunit.hrl").

start() ->
    Keys = [crypto:rand_bytes(32)],
    Interfaces = [{{127,0,0,1}, 5555}, {{127,0,0,1}, 5556},
		  {{127,0,0,1}, 5557}, {{127,0,0,1}, 5558},
		  {{127,0,0,1}, 5559}],
    start_members(Keys, Interfaces).

stop(Pids) ->
    _ = [swim:stop(P) || P <- Pids],
    ok.

start_members(Keys, [{Address, Port} | Rest]) ->
    {ok, Swim} = swim:start_link({Address, Port}, Keys, [{protocol_period, 500}]),
    start_other_members([{Address, Port}], Keys, Rest, [Swim]).

start_other_members(_Seeds, _Keys, [], Acc) ->
    Acc;
start_other_members(Seeds, Keys, [{Address, Port} | Rest], Acc) ->
    {ok, Swim} = swim:start_link({Address, Port}, Keys,
					   [{seeds, Seeds},
					    {protocol_period, 500}]),
    start_other_members(Seeds, Keys, Rest, [Swim | Acc]).

swim_gossip_group_test_() ->
    {timeout, 60,
     {setup,
      fun start/0,
      fun stop/1,
      fun(Pids) ->
	      ok = timer:sleep(15000),
	      [First | Rest] = lists:map(
				 fun(Pid) ->
					 Ms = swim:members(Pid),
					 L = swim:local_member(Pid),
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
