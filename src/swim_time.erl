-module(swim_time).

-export([send_after/3]).
-export([cancel_timer/1]).
-export([cancel_timer/2]).
-export([monotonic_time/0]).

send_after(Time, Dest, Msg) ->
    erlang:send_after(Time, Dest, Msg).

cancel_timer(TRef) ->
    erlang:cancel_timer(TRef).

cancel_timer(TRef, Options) ->
    erlang:cancel_timer(TRef, Options).

monotonic_time() ->
    erlang:monotonic_time().
