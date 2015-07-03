%%% ----------------------------------------------------------------------------
%%% Copyright (c) 2015. All Rights Reserved.
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

-module(swim_gossip_tests).

-include_lib("eunit/include/eunit.hrl").

-define(wait(T), receive
		     ok -> ok
		 after
		     T -> ok
		 end).

-define(wait_for_msg(M, T),
	       fun Loop() ->
		       receive
			   M -> ok;
			   R -> Loop()
		       after
			   T -> timeout
		       end
	       end()).

start() ->
    {ok, _Apps} = application:ensure_all_started(swim),
    {ok, _Pid} = damocles:start(),
    Opts = [{protocol_period, 500}, {ack_timeout, 100},
	    {keys, [crypto:strong_rand_bytes(32)]}],
    start_gossip_members(5, Opts).

stop(_) ->
    ok = application:stop(swim),
    damocles:stop().

start_gossip_members(N, Opts) ->
    _ = [damocles:add_interface("10.10.10." ++ integer_to_list(10 + I)) ||
	    I <- lists:seq(0, N)],
    {ok, _} = swim:start_gossip(lan_0, {10,10,10,10}, 5000, Opts),
    {Names, Members} = lists:unzip(
			 lists:map(start_member(Opts), lists:seq(1, N))),
    {[lan_0 | Names], [{{10,10,10,10}, 5000} | Members]}.

start_member(Opts) ->
    fun(I) ->
	    Name = list_to_atom("lan_" ++ integer_to_list(I)),
	    Ip = {10,10,10,10 + I},
	    Port = 5000,
	    {ok, _} = swim:start_gossip(Name, Ip, Port,
					[{seeds, [{{10,10,10,10}, 5000}]} | Opts]),
	    {Name, {Ip, Port}}
    end.

swim_gossip_network_test_() ->
    {setup,
     fun start/0, fun stop/1,
     fun(GossipGroup) ->
	     [{timeout, 60, ?_assert(stable_membership_with_high_latency(GossipGroup))},
	      {timeout, 60, ?_assert(stable_membership_with_packet_loss(GossipGroup))},
	      {timeout, 60, ?_assert(stable_membership_with_partition(GossipGroup))}]
     end}.

stable_membership_with_partition({Names, Members}) ->
    Ips = lists:map(fun({{A1, A2, A3, A4}, _Port}) ->
		      lists:flatten(io_lib:format("~p.~p.~p.~p", [A1, A2, A3, A4]))
	      end, Members),
    Partition1 = lists:sublist(Ips, 4, 2),
    Partition2 = lists:sublist(Ips, 1, 2),
    _ = damocles:isolate_between_interfaces(Partition1, Partition2),
    ?wait(10000),
    ok = swim:subscribe(hd(lists:reverse(Names)), user),
    _ = swim:publish(hd(Names), test),
    ok = ?wait_for_msg({swim, {user, test}}, 1000),
    is_stable(Names, Members).

stable_membership_with_packet_loss({Names, Members}) ->
    ok = damocles:packet_loss_global(0.05),
    ?wait(10000),
    is_stable(Names, Members).

stable_membership_with_high_latency({Names, Members}) ->
    ok = damocles:delay_global(75),
    ?wait(10000),
    is_stable(Names, Members).

is_stable(Names, Members) ->
    lists:all(fun(N) ->
		      lists:sort(Members) == lists:sort(swim:members(N))
	      end, Names).
