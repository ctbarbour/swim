%%% ----------------------------------------------------------------------------
%%% Copyright (c) 2015-2017 All Rights Reserved.
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

%%% @copyright 2015-2017
%%% @version {@version}

%%% @doc This module is responsible for maintaining membership updates and user
%%% provided events along with their state as a part of the
%%% infection-style dissemination component of the SWIM protocol.
%%%
%%% ### Infection-Style Dissemination
%%% As an alternative to IP Multicast
%%% or a point-to-point messaging scheme the SWIM protocol
%%% disseminates membership updates by piggybacking on messages sent
%%% as a part of the failure detection protocol. Thus, implementation
%%% does not generate any extra packets to send membership updates.
%%%
%%% Here, `swim_broadcasts' maintains a buffer of recent membership
%%% events along with a count for each event. The local count
%%% specifies the number of times the event has been piggybacked so
%%% far by this member and is used to choose which events to piggyback
%%% next. Each event is piggybacked at most `Retransmit * log(N +
%%% 1)' times, where `Retransmit' is a configurable parameter.
%%% If the size of events in the buffer is larger than the maximum number of
%%% events that can be piggybacked on a single PING or ACK, events that have
%%% been gossiped fewer times are preferred. This is needed as the
%%% protocol period is fixed and the rate of membership changes might
%%% temporarily overwhelm the speed of dissemination. Preferring
%%% "younger" events under such circumstances ensures that all
%%% membership changes infect at least a few members - when the
%%% membership change rate quiesces, older events will
%%% propagate through the rest of the gossip group. Membership events are always
%%% preferred over user-provided events.
%%%
%%% @TODO: Consider a more efficient implementation. We're sorting lists a lot.
%%% We could potentially use a min-heap or priority queue but need to handle the requirement for
%%% invalidating events about the same member. Consider
%%% https://github.com/okeuday/pqueue/blob/master/src/pqueue4.erl
%%% @end
-module(swim_broadcasts).

-export([new/1]).
-export([new/2]).
-export([insert/2]).
-export([take/2]).
-export([prune/2]).
-export([takefold/3]).
-export([take_limit/1]).
-export([take_limit/2]).
-export([retransmit_limit/2]).

-record(broadcast, {
          members     = []  :: [{non_neg_integer(), swim:membership_event()}],
          users       = []  :: [{non_neg_integer(), swim:user_event()}],
          retransmits       :: pos_integer(),
          limit_fun         :: fun((swim:member_event() | swim:user_event()) -> pos_integer()),
          limit             :: pos_integer()
         }).

-opaque broadcast() :: #broadcast{}.
-export_type([broadcast/0]).

-spec new(Retransmits) -> Broadcast when
      Retransmits :: pos_integer(),
      Broadcast   :: broadcast().

new(Retransmits) ->
    new(Retransmits, default_limit()).

-spec new(Retransmits, Limit) -> Broadcast when
      Retransmits :: pos_integer(),
      Limit       :: pos_integer(),
      Broadcast   :: broadcast().

new(Retransmits, Limit) ->
    LimitFun = default_limit_fun(),
    #broadcast{retransmits = Retransmits, limit = Limit, limit_fun = LimitFun}.

default_limit() ->
    swim_messages:event_size_limit().

default_limit_fun() ->
    fun(E) -> iolist_size(swim_messages:encode_event(E)) end.

-spec retransmit_limit(NumMembers, Broadcast) -> Limit when
      NumMembers       :: pos_integer(),
      Broadcast        :: broadcast(),
      Limit            :: pos_integer().

retransmit_limit(NumMembers, #broadcast{retransmits = Factor}) ->
    round(math:log(NumMembers + 1)) + Factor.

-spec take(Max, Broadcasts0) -> {Events, Broadcasts} when
      Max         :: pos_integer(),
      Broadcasts0 :: broadcast(),
      Events      :: [swim:event()],
      Broadcasts  :: broadcast().

take(Max, #broadcast{members = MembershipEvents0, users = UserEvents0} = Broadcast) ->
    {B, M, U} = take(Max, MembershipEvents0, UserEvents0, {[], [], []}),
    {B, Broadcast#broadcast{members = M, users = U}}.

take(0, MembershipEvents, UserEvents, {B, M, U}) ->
    {B, lists:sort(MembershipEvents ++ M), lists:sort(UserEvents ++ U)};
take(_K, [], [], {B, M, U}) ->
    {B, lists:sort(M), lists:sort(U)};
take(K, [{T, Event} | Events], UserEvents, {B, M, U}) ->
    take(K - 1, Events, UserEvents, {[{membership, Event} | B], [{T + 1, Event} | M], U});
take(K, [], [{T, Event} | Events], {B, M, U}) ->
    take(K - 1, [], Events, {[{user, Event} | B], M, [{T + 1, Event} | U]}).

-spec take_limit(Broadcasts0) -> {Events, Broadcasts} when
      Broadcasts0 :: broadcast(),
      Events      :: [swim:membership_event() | swim:user_event()],
      Broadcasts  :: broadcast().

take_limit(#broadcast{limit = Limit, limit_fun = Fun, members = Members, users = Users} = Broadcast) ->
    {B, M, U} = take_limit(Limit, Fun, Members, Users, {[], [], []}),
    {B, Broadcast#broadcast{members = M, users = U}}.

take_limit(0, _Fun, Members, Users, {B, M, U}) ->
    {B, lists:sort(Members ++ M), lists:sort(Users ++ U)};
take_limit(_Limit, _Fun, [], [], {B, M, U}) ->
    {B, lists:sort(M), lists:sort(U)};
take_limit(Limit, Fun, [{T, E} | Members], Users, {B, M, U}) ->
    case Limit - Fun(E) of
        L when L >= 0 ->
            take_limit(L, Fun, Members, Users, {[{membership, E} | B], [{T + 1, E} | M], U});
        _ ->
            take_limit(0, Fun, Members, Users, {B, [{T, E} | Members], U})
    end;
take_limit(Limit, Fun, [], [{T, E} | Users], {B, M, U}) ->
    case Limit - Fun(E) of
        L when L >= 0 ->
            take_limit(L, Fun, [], Users, {[{user, E} | B], M, [{T + 1, E} | U]});
        _ ->
            take_limit(0, Fun, [], Users, {B, M, [{T, E} | U]})
    end.

-spec take_limit(Member, Broadcasts0) -> {Events, Broadcasts} when
      Member      :: swim:member(),
      Broadcasts0 :: broadcast(),
      Events      :: [swim:membership_event() | swim:user_event()],
      Broadcasts  :: broadcast().

take_limit(Target, Broadcasts) ->
    #broadcast{limit = Limit, limit_fun = Fun, users = Users} = Broadcasts,
    Partition = fun({_, {suspect, _, M}}) -> M =:= Target end,
    {Maybe, Members} = lists:partition(Partition, Broadcasts#broadcast.members),
    {B, M, U} =
        case Maybe of
            [] ->
                take_limit(Limit, Fun, Members, Users, {[], [], []});
            [{T, About}] ->
                Acc = {[About], [{T + 1, About}], []},
                take_limit(Limit - Fun(About), Fun, Members, Users, Acc)
        end,
    {B, Broadcasts#broadcast{members = M, users = U}}.

-spec takefold(Fun, InitAcc, Broadcast0) -> {Acc, Broadcast} when
      Fun        :: fun((swim:membership_event() | swim:user_event(), InitAcc) -> Acc),
      InitAcc    :: any(),
      Acc        :: InitAcc,
      Broadcast0 :: broadcast(),
      Broadcast  :: broadcast().

takefold(Fun, InitAcc, #broadcast{members = Members, users = Users} = Broadcast) ->
    {Acc, M, U} = takefold(Fun, Members, Users, {InitAcc, [], []}),
    {Acc, Broadcast#broadcast{members = M, users = U}}.

takefold(_Fun, [], [], {Acc, M, U}) ->
    {Acc, lists:sort(M), lists:sort(U)};
takefold(Fun, [{T, E} | Members], Users, {Acc0, M, U}) ->
    case Fun({membership, E}, Acc0) of
        {take, Acc} ->
            takefold(Fun, Members, Users, {Acc, [{T + 1, E} | M], U});
        skip ->
            takefold(Fun, Members, Users, {Acc0, M, [{T, E} | U]})
    end;
takefold(Fun, [], [{T, E} | Users], {Acc0, M, U}) ->
    case Fun({user, E}, Acc0) of
        {take, Acc} ->
            takefold(Fun, [], Users, {Acc, M, [{T + 1, E} | U]});
        skip ->
            takefold(Fun, [], Users, {Acc0, M, [{T, E} | U]})
    end.

-spec prune(Retransmits, Broadcasts0) -> Broadcasts when
      Retransmits :: non_neg_integer(),
      Broadcasts0 :: broadcast(),
      Broadcasts  :: broadcast().

prune(Retransmits, #broadcast{members = MembershipEvents0, users = UserEvents0} = Broadcast) ->
    Filter = fun({T, _}) -> T < Retransmits end,
    MembershipEvents = lists:filter(Filter, MembershipEvents0),
    UserEvents = lists:filter(Filter, UserEvents0),
    Broadcast#broadcast{members = MembershipEvents, users = UserEvents}.

%% @doc Insert an Event or list of Events to the Broadcast queue
%%
%% Upon inserting a Membership Event we invalidate any existing event about the same target member
%% to prevent the brodcast of stale information.
%% @end
-spec insert(Events, Broadcasts0) -> Broadcasts when
      Events      :: swim:swim_event() | [swim:swim_event()],
      Broadcasts0 :: broadcast(),
      Broadcasts  :: broadcast().

insert(Events, Broadcast) when is_list(Events) ->
    lists:foldl(fun insert/2, Broadcast, Events);
insert({membership, Event}, #broadcast{members = MembershipEvents} = Broadcast) ->
    Broadcast#broadcast{members = invalidate(Event, MembershipEvents)};
insert({user, Event}, #broadcast{users = UserEvents} = Broadcast) ->
    Broadcast#broadcast{users = lists:sort([{0, Event} | UserEvents])}.

-spec invalidate(Event, Events0) -> Events when
      Event   :: swim:membership_event(),
      Events0 :: [{non_neg_integer(), swim:membership_event()}],
      Events  :: [{non_neg_integer(), swim:membership_event()}].

invalidate({_, _, Member} = Event, Events) ->
    invalidate(Member, Event, Events);
invalidate({_, _, Member, _} = Event, Events) ->
    invalidate(Member, Event, Events).

-spec invalidate(Member, Event, Events0) -> Events when
      Member  :: swim:member(),
      Event   :: swim:membership_event(),
      Events0 :: [{non_neg_integer(), swim:membership_event()}],
      Events  :: [{non_neg_integer(), swim:membership_event()}].

invalidate(Member, Event, Events) ->
    Filter = fun({_, {_, _, M}}) -> M =/= Member;
                ({_, {_, _, M, _}}) -> M =/= Member end,
    lists:sort([{0, Event} | lists:filter(Filter, Events)]).
