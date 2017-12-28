-module(swim_pushpull_sup).
-behavior(supervisor).

-export([start_link/2]).
-export([init/1]).

start_link(IpAddr, Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [IpAddr, Port]).

init([IpAddr, Port]) ->
    ListenerSpec = #{
      id => pushpull,
      start => {swim_pushpull, start_link, [IpAddr, Port, #{}]}},
    Flags = #{strategy => one_for_one,
              intensity => 10,
              period => 10},
    {ok, {Flags, [ListenerSpec]}}.
