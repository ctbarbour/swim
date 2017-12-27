-module(swim_pushpull_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, Port} = application:get_env(swim, port),
    RanchSupSpec = #{id => ranch_sup,
                     start => {ranch_sup, start_link, []},
                     restart => permanent,
                     shutdown => 5000,
                     type => supervisor,
                     modules => [ranch_sup]},
    Opts = [{port, Port}, {packet, 4}],
    ListenerSpec = ranch:child_spec(swim_pushpull, ranch_tcp, Opts, swim_pushpull, []),
    Flags = #{strategy => one_for_one,
              intensity => 10,
              period => 10},
    {ok, {Flags, [RanchSupSpec, ListenerSpec]}}.
