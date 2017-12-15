-module(swim).

-export([join/1]).
-export([leave/0]).
-export([members/0]).
-export([myself/0]).

join(Seeds) ->
    swim_state:join(Seeds).

leave() ->
    swim_state:leave().

members() ->
    swim_state:members().

myself() ->
    swim_state:local_member().
