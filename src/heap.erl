%% Taken from https://github.com/qhool/heap
-module(heap).

-export([new/0,new/1,
         insert/2, append/2,
         min/1,max/1,
         take_min/1,take_max/1,
         from_list/1, from_list/2,
         to_list/1, to_list/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-export([print/1,test_list/2,insert_test/2,take_test/2,
         take_bottom/1,listmost/2]).
-endif.

new() ->
    new(max).
new(Type) ->
    {heap,Type,0,{}}.

insert({heap,Type,Size,Heap},Item) ->
    {heap,Type,Size+1,insert(Type,dirs(Size+1),Heap,Item)}.

insert(_Type,_D,{},Item) ->
    {Item};
insert(Type,_D,{Parent},Item) ->
    siftl(Type,{Parent,{Item},{}});
insert(Type,[0|D],{Parent,Left,Right}=_H,Item) ->
    siftl(Type,{Parent,insert(nexttype(Type),D,Left,Item),Right});
insert(Type,[1|D],{Parent,Left,Right}=_H,Item) ->
    siftr(Type,{Parent,Left,insert(nexttype(Type),D,Right,Item)}).

append(Heap,[]) ->
    Heap;
append(Heap,[A|As]) ->
    append(insert(Heap,A),As).


min(H) ->
    most(min,keep,H).

max(H) ->
    most(max,keep,H).

take_min(H) ->
    most(min,take,H).

take_max(H) ->
    most(max,take,H).

from_list(L) ->
    from_list(max,L).

from_list(Type,L) ->
    append(heap:new(Type),L).

to_list({heap,Type,_,_}=H) ->
    to_list(optype(Type),H).

to_list(Mode,H) ->
    lists:reverse(to_list(Mode,H,[])).

to_list(_,{heap,_,_,{}},Out) ->
    Out;
to_list(Mode,H,Out) ->
    {ok,Item,H1} = most(Mode,take,H),
    to_list(Mode,H1,[Item|Out]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Internal Functions
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
top({}) ->
    none;
top({P}) ->
    {ok,P};
top({P,_,_}) ->
    {ok,P}.

-define(IsMax(Type),(Type =:= max orelse Type =:= mmax)).
-define(IsMin(Type),(Type =:= min orelse Type =:= mmin)).
-define(hasv(H),(size(H) > 0)).
-define(v_(H,Tag),(case size(H) of
                  0 -> none;
                  _ -> {element(1,H),Tag}
                  end)).
-define(v_i(H,A,Tag),if size(H) >= A -> ?v_(element(A,H),{Tag,A});
                         true -> none
                     end).

-define(v_l(H,Tag),?v_i(H,2,Tag)).
-define(v_r(H,Tag),?v_i(H,3,Tag)).

-define(ss(Fun,Type,H,Item),Fun(Type,setelement(1,H,Item))).

-define(ss(Fun,Type,N,H,Item),setelement(N,H,?ss(Fun,Type,element(N,H),Item))).

-define(ss1(H,Item),?ss(siftdown1,Type,H,Item)).
-define(ss2(H,Item),?ss(siftdown2,Type,H,Item)).
-define(ss2(N,H,Item),?ss(siftdown2,Type,N,H,Item)).

most(min,_,{heap,max,_,_}) ->
    {error,max};
most(max,_,{heap,min,_,_}) ->
    {error,min};
most(_Mode,_Take,{heap,_,0,{}}) ->
    empty;
most(_Mode,keep,{heap,_Type,1,{I}}) ->
    {ok,I};
most(_Mode,take,{heap,Type,1,{I}}) ->
    {ok,I,{heap,Type,0,{}}};
most(Mode,Take,{heap,Type,_Size,{P,_,_}}=H) when Mode == min, ?IsMin(Type);
                                                Mode == max, ?IsMax(Type) ->
    case Take of
        keep -> {ok,P};
        take ->
            {{heap,_,Size1,Heap},Bottom} = take_bottom(H),
            {ok,P,{heap,Type,Size1,siftdown(Type,replace_top(Heap,Bottom))}}
    end;
most(Mode,Take,{heap,Type,_Size,{_P,L,R}}=H) ->
    {Item,Which} =
        case {top(L),top(R)} of
            {{ok,LI},{ok,RI}} ->
                erlang:Mode({LI,left},{RI,right});
            {{ok,LI},none} -> {LI,left}
        end,
    case Take of
        take ->
            case {Which,take_bottom(H)} of
                {_,{{heap,_,_,{_}}=H1,_Bottom}} ->
                    {ok,Item,H1};
                {right,{{heap,_,_,{_,_,{}}}=H1,_Bottom}} ->
                    {ok,Item,H1};
                {_,{{heap,_,Size1,{P1,L1,R1}},Bottom}} ->
                    Heap1 =
                        case Which of
                            left ->
                                {P1,siftdown(nexttype(Type),replace_top(L1,Bottom)),R1};
                            right ->
                                {P1,L1,siftdown(nexttype(Type),replace_top(R1,Bottom))}
                        end,
                    {ok,Item,{heap,Type,Size1,Heap1}}
            end;
        keep ->
            {ok,Item}
    end.

-define(SWAP(Type,Parent,Item),
        ((?IsMax(Type) andalso Parent < Item) orelse
         (?IsMin(Type) andalso Parent > Item))).
-define(MMSWAP(Type,Grandparent,Item),
        ((Type =:= mmax andalso Grandparent < Item) orelse
         (Type =:= mmin andalso Grandparent > Item))).

siftl(Type,{Parent,{LI},R}) when ?SWAP(Type,Parent,LI) ->
    {LI,{Parent},R};
siftl(Type,{Parent,{LI,LL,LR},R}) when ?SWAP(Type,Parent,LI) ->
    {LI,{Parent,LL,LR},R};
siftl(Type,X) when Type =:= min; Type =:= max ->
    X;
siftl(Type,{Parent,{LI,{LL},LR},R}) when ?MMSWAP(Type,Parent,LL) ->
    {LL,{LI,{Parent},LR},R};
siftl(Type,{Parent,{LI,LL,{LR}},R}) when ?MMSWAP(Type,Parent,LR) ->
    {LR,{LI,LL,{Parent}},R};
siftl(Type,{Parent,{LI,{LL,LLL,LLR},LR},R}) when ?MMSWAP(Type,Parent,LL) ->
    {LL,{LI,{Parent,LLL,LLR},LR},R};
siftl(Type,{Parent,{LI,LL,{LR,LRL,LRR}},R}) when ?MMSWAP(Type,Parent,LR) ->
    {LR,{LI,LL,{Parent,LRL,LRR}},R};
siftl(_,X) ->
    X.

siftr(Type,{Parent,L,{RI}}) when ?SWAP(Type,Parent,RI) ->
    {RI,L,{Parent}};
siftr(Type,{Parent,L,{RI,RL,RR}}) when ?SWAP(Type,Parent,RI) ->
    {RI,L,{Parent,RL,RR}};
siftr(Type,X) when Type =:= min; Type =:= max ->
    X;
siftr(Type,{Parent,L,{RI,{RL},RR}}) when ?MMSWAP(Type,Parent,RL) ->
    {RL,L,{RI,{Parent},RR}};
siftr(Type,{Parent,L,{RI,RL,{RR}}}) when ?MMSWAP(Type,Parent,RR) ->
    {RR,L,{RI,RL,{Parent}}};
siftr(Type,{Parent,L,{RI,{RL,RLL,RLR},RR}}) when ?MMSWAP(Type,Parent,RL) ->
    {RL,L,{RI,{Parent,RLL,RLR},RR}};
siftr(Type,{Parent,L,{RI,RL,{RR,RRL,RRR}}}) when ?MMSWAP(Type,Parent,RR) ->
    {RR,L,{RI,RL,{Parent,RRL,RRR}}};
siftr(_,X) ->
    X.

siftdown(MMType,H) when MMType =:= mmin; MMType =:= mmax ->
    siftdown2(MMType,H);
siftdown(Type,H) ->
    siftdown1(Type,H).

siftdown1(_,{P}) ->
    {P};
siftdown1(Type,{P,L,R}=Heap) ->
    case listmost(Type,[{P,top},?v_(L,l),?v_(R,r)]) of
        {_,top} ->
            Heap;
        {V,l} ->
            {V,?ss1(L,P),R};
        {V,r} ->
            {V,L,?ss1(R,P)}
    end.

siftdown2(_Type,{P}) ->
    {P};
siftdown2(Type,{P,L,R}=Heap)  ->
    case listmost(Type,[{P,top},?v_(L,l),?v_(R,r)]) of
        {_,top} ->
            case listmost(Type,[{P,top},?v_l(L,l),?v_r(L,l),?v_l(R,r),?v_r(R,r)]) of
                {_,top} ->
                    Heap;
                {V,{l,N}} ->
                    {V,?ss2(N,L,P),R};
                {V,{r,N}} ->
                    {V,L,?ss2(N,R,P)}
            end;
        {V,l} ->
            siftdown2(Type,{V,replace_top(L,P),R});
        {V,r} ->
            siftdown2(Type,{V,L,replace_top(R,P)})
    end.

listmost(Type,L) ->
    listmost(optype(Type),L,none).

listmost(_Type,[],Most) ->
    Most;
listmost(Type,[none|Xs],Most) ->
    listmost(Type,Xs,Most);
listmost(Type,[X|Xs],none) ->
    listmost(Type,Xs,X);
listmost(min,[{VX,_}=X|Xs],{VMost,_}) when VX < VMost ->
    listmost(min,Xs,X);
listmost(max,[{VX,_}=X|Xs],{VMost,_}) when VX > VMost ->
    listmost(max,Xs,X);
listmost(Type,[_|Xs],Most) ->
    listmost(Type,Xs,Most).

take_bottom({heap,Type,Size,Heap}) ->
    {Heap1,Item} = take_bottom(dirs(Size),Heap),
    {{heap,Type,Size-1,Heap1},Item}.

take_bottom(_D,{Item}) ->
    {{},Item};
take_bottom(_D,{Parent,{Item},{}}) ->
    {{Parent},Item};
take_bottom([0|D],{Parent,L,R}) ->
    {L1,Item} = take_bottom(D,L),
    {{Parent,L1,R},Item};
take_bottom([1|D],{Parent,L,R}) ->
    {R1,Item} = take_bottom(D,R),
    {{Parent,L,R1},Item}.

replace_top({_P},Item) ->
    {Item};
replace_top({_P,L,R},Item) ->
    {Item,L,R}.

dirs(0) -> [];
dirs(Size) ->
    dirs(Size,[]).
dirs(1,Out) ->
    Out;
dirs(Size,Out) ->
    dirs(Size div 2,[Size rem 2|Out]).

nexttype(min) ->
    min;
nexttype(max) ->
    max;
nexttype(mmin) ->
    mmax;
nexttype(mmax) ->
    mmin.

optype(mmin) ->
    min;
optype(mmax) ->
    max;
optype(Type) ->
    Type.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Tests
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

print(Heap) ->
    print(standard_io,Heap).
print(F,{heap,_,_,Heap}) ->
    print(F,Heap,[]);
print(F,Heap) ->
    print(F,Heap,[]).

print(F,{P,L,R},In) ->
    print(F,R,"   " ++ In),
    io:format(F,"~s~3w~n",[In,P]),
    print(F,L,"   " ++ In);
print(F,{P},In) ->
    io:format(F,"~s~3w~n",[In,P]);
print(F,{},In) ->
    io:format(F,"~s~3w~n",[In,{}]).

checkup({heap,Type,Size,H}) ->
    case catch(checkup(Type,H)) of
        {error,_} = E -> E;
        {_,_,ActualSize} ->
            if Size /= ActualSize ->
                    {error,{bad_size,{actual,ActualSize}}};
               true ->
                    ok
            end
    end;
checkup(_) ->
    {error,not_a_heap}.

checkup(_Type,{}) ->
    {none,none,0};
checkup(_Type,{I}) ->
    {I,I,1};
checkup(Type,{P,L,R}) ->
    {LMin,LMax,LSize} = checkup(nexttype(Type),L),
    {RMin,RMax,RSize} = checkup(nexttype(Type),R),
    case optype(Type) of
        min when LMin /= none, P > LMin ->
            throw({error,bad_invariant,{P,'>',LMin}});
        min when RMin /= none, P > RMin ->
            throw({error,bad_invariant,{P,'>',RMin}});
        max when LMax /= none, P < LMax ->
            throw({error,bad_invariant,{P,'<',LMax}});
        max when RMax /= none, P < RMax ->
            throw({error,bad_invariant,{P,'<',RMax}});
        _ -> ok
    end,
    Min = lists:min([P,LMin,RMin]),
    Max = lists:max([P,LMax,RMax]),
    {Min,Max,1 + LSize + RSize}.

insert_test(Type,List) when is_atom(Type) ->
    insert_test(new(Type),List);
insert_test(Heap,[X|Xs]) ->
    Heap1 = insert(Heap,X),
    case checkup(Heap1) of
        ok -> ok;
        E ->
            io:format("Heap failed checks after inserting ~p: ~p~n",[X,E]),
            io:format("Was: ~n"),
            print(Heap),
            io:format("After: ~n"),
            print(Heap1),
            throw(E)
    end,
    insert_test(Heap1,Xs);
insert_test(Heap,[]) ->
    %%print(Heap),
    Heap.

insert_take_test_() ->
    {inparallel,
     [ {generator,
        fun() ->
                insert_take_test_gen(ListKind)
        end}
       || ListKind <- [ sequence, reverse, interleave, reverse_interleave,
                        {random,7},{random,10},{random,13},
                        {random,20},{random,33},{random,100},{random,127},
                        {random,144},{random,241},{random,569},
                        {random,777},{random,1000},
                        shuffle ] ]}.
insert_take_test_gen(ListKind) ->
    {inparallel,
     [ { string:join([atom_to_list(HeapType),atom_to_list(Fn),
                      integer_to_list(length(List)),ListType]," "),
         case Fn of
             insert ->
                 fun() ->
                         insert_test(HeapType,List)
                 end;
             take ->
                 fun() ->
                         take_test(optype(HeapType),
                                   from_list(HeapType,List))
                 end
         end
       }
       || Fn <- [insert,take],
          HeapType <- [ min, max, mmax, mmin ],
          {ListType,List} <- test_lists(ListKind) ]}.

steady_test_() ->
    {inparallel,
     [ { string:join([ atom_to_list(HeapType), integer_to_list(Size),
                       "for", integer_to_list(N), "steps" ], " "),
         fun() ->
                 steady_test(HeapType,Size,N)
         end }
       || HeapType <- [ mmax, mmin ],
          Size <- lists:seq(2,200,7),
          N <- [100,500,999] ]}.

take_test(Mode,H) ->
    take_test(Mode,H,none).

take_test(_,{heap,_,_,{}},_) ->
    io:format("all done~n");
take_test(Mode,H,Last) ->
    %%io:format("~n--------------------------------~n~n"),
    %%print(H),
    {ok,Item,H1} = most(Mode,take,H),
    %%io:format("Took ~p~n------------------------~n~n",[Item]),
    case checkup(H1) of
        ok -> ok;
        E ->
            io:format("Heap failed checks after taking ~p: ~p~n",[Item,E]),
            io:format("Was: ~n"),
            print(H),
            io:format("After: ~n"),
            print(H1),
            io:format("Call was:~nheap:take_~w(~w)~n",[Mode,H]),
            throw(E)
    end,
    case Mode of
        _ when Last == none -> ok;
        max when Last >= Item -> ok;
        min when Last =< Item -> ok;
        _ -> throw(take_out_of_order)
    end,
    take_test(Mode,H1,Item).

steady_test(Mode,Size,N_Steps) ->
    InitList = test_list({random,Size},Size),
    Heap = from_list(Mode,InitList),
    SortList = lists:sort(InitList),
    steady_test(Heap,Mode,if Mode == mmin -> SortList;
                             true -> lists:reverse(SortList)
                          end, Size, N_Steps).

steady_test(Heap,Mode,List,_,0) ->
    List = to_list(optype(Mode),Heap);
steady_test(Heap,Mode,List,Size,N) ->
    io:format("Mode: ~p~n",[Mode]),
    {ok,Item,Heap1} = most(optype(Mode),take,Heap),
    [Item|List1] = List,
    NewItem = crypto:rand_uniform(1,Size),
    List2 = case Mode of
                mmin -> lists:reverse(ins_list(List1,NewItem));
                mmax -> ins_list(lists:reverse(List1),NewItem)
            end,
    Heap2 = insert(Heap1,NewItem),
    steady_test(Heap2,nexttype(Mode),List2,Size,N-1).

ins_list(List,Item) ->
    ins_list(List,Item,[]).
ins_list([T|Tail],Item,Head) when Item > T ->
    ins_list(Tail,Item,[T|Head]);
ins_list(Tail,Item,Head) ->
    lists:reverse(Head) ++ [Item|Tail].

test_lists(Type) ->
    [ {unicode:characters_to_list(io_lib:format("~p.~p",[Type,N])),test_list(Type,N)}
      || N <- lists:seq(1,250) ].
test_list(sequence,N) ->
    lists:seq(1,N);
test_list(reverse,N) ->
    lists:seq(N,1,-1);
test_list(interleave,N) ->
    lists:flatten([ tuple_to_list(T)
                    || T <- lists:zip(lists:seq(1,N,2),lists:seq(N,1,-2)) ]);
test_list(reverse_interleave,N) ->
    lists:flatten([ tuple_to_list(T)
                    || T <- lists:zip(lists:seq(N,1,-2),lists:seq(1,N,2)) ]);
test_list({random,R},N) ->
    [ crypto:rand_uniform(1,R) || _ <- lists:seq(1,N) ];
test_list(shuffle,N) ->
    [ X || {_,X} <- lists:sort( lists:zip( test_list({random,N*100},N),
                                           lists:seq(1,N) ) ) ].

-endif. %%TEST
