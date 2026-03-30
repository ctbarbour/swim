-module(prop_swim_awareness).

-include_lib("proper/include/proper.hrl").

-export([prop_awareness/0]).

g_max() ->
    range(1, 100).

g_timeout() ->
    range(1, 10000).

g_operations() ->
    list(oneof([success, failure])).

g_awareness() ->
    ?LET({Max, Ops}, {g_max(), g_operations()},
         lists:foldl(fun(success, A) -> swim_awareness:success(A);
                        (failure, A) -> swim_awareness:failure(A)
                     end, swim_awareness:new(Max), Ops)).

g_awareness(Max) ->
    ?LET(Ops, g_operations(),
         lists:foldl(fun(success, A) -> swim_awareness:success(A);
                        (failure, A) -> swim_awareness:failure(A)
                     end, swim_awareness:new(Max), Ops)).

prop_awareness() ->
    conjunction([
                 {new_scales_to_base, prop_new_scales_to_base()},
                 {bounded, prop_bounded()},
                 {success_decreases, prop_success_decreases()},
                 {failure_increases, prop_failure_increases()},
                 {failure_n_equiv, prop_failure_n_equiv()},
                 {scale_linear, prop_scale_linear()}
                ]).

prop_new_scales_to_base() ->
    ?FORALL({Max, Timeout}, {g_max(), g_timeout()},
            swim_awareness:scale(Timeout, swim_awareness:new(Max)) =:= Timeout).

prop_bounded() ->
    ?FORALL(Max, g_max(),
            ?FORALL(A, g_awareness(Max),
                    begin
                        V = swim_awareness:scale(1, A),
                        V >= 1 andalso V =< Max + 1
                    end)).

prop_success_decreases() ->
    ?FORALL(A, g_awareness(),
            swim_awareness:scale(1, swim_awareness:success(A)) =<
            swim_awareness:scale(1, A)).

prop_failure_increases() ->
    ?FORALL(A, g_awareness(),
            swim_awareness:scale(1, swim_awareness:failure(A)) >=
            swim_awareness:scale(1, A)).

prop_failure_n_equiv() ->
    ?FORALL({Max, N}, {g_max(), range(1, 50)},
            begin
                A = swim_awareness:new(Max),
                ByN = swim_awareness:failure(N, A),
                ByFold = lists:foldl(fun(_, Acc) -> swim_awareness:failure(Acc) end,
                                     A, lists:seq(1, N)),
                swim_awareness:scale(1, ByN) =:= swim_awareness:scale(1, ByFold)
            end).

prop_scale_linear() ->
    ?FORALL({T1, T2, A}, {g_timeout(), g_timeout(), g_awareness()},
            swim_awareness:scale(T1 + T2, A) =:=
            swim_awareness:scale(T1, A) + swim_awareness:scale(T2, A)).
