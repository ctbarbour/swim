-module(swim_pushpull_sup).
-behavior(supervisor).

-export([start_link/2, start_link/3]).
-export([init/1]).

start_link(IpAddr, Port) ->
    start_link(default, IpAddr, Port).

start_link(Name, IpAddr, Port) ->
    supervisor:start_link({local, swim_name:proc_name(Name, pushpull_sup)}, ?MODULE, [Name, IpAddr, Port]).

init([Name, IpAddr, Port]) ->
    ListenerSpec = #{
      id => pushpull,
      start => {swim_pushpull, start_link, [Name, IpAddr, Port, #{}]}},
    Flags = #{strategy => one_for_one,
              intensity => 10,
              period => 10},
    {ok, {Flags, [ListenerSpec]}}.
