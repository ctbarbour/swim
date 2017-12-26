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

-export([new/0]).
-export([insert/2]).
-export([take/2]).
-export([prune/2]).
-export([retransmit_limit/2]).

-type queue(E)      :: [{non_neg_integer(), E}].
-opaque broadcast() :: {queue(swim:membership_event()),
                        queue(swim:user_event())}.
-export_type([broadcast/0]).

-spec new() -> Broadcast when Broadcast :: broadcast().

new() ->
    {[], []}.

-spec retransmit_limit(NumMembers, RetransmitFactor) -> Limit when
      NumMembers       :: pos_integer(),
      RetransmitFactor :: pos_integer(),
      Limit            :: pos_integer().

retransmit_limit(NumMembers, RetransmitFactor) ->
    round(math:log(NumMembers + 1)) + RetransmitFactor.

-spec take(Max, Broadcasts0) -> {Events, Broadcasts} when
      Max         :: pos_integer(),
      Broadcasts0 :: broadcast(),
      Events      :: [swim:event()],
      Broadcasts  :: broadcast().

take(Max, {MembershipEvents0, UserEvents0}) ->
    take(Max, MembershipEvents0, UserEvents0, {[], [], []}).

take(0, MembershipEvents, UserEvents, {B, M, U}) ->
    {B, {lists:sort(MembershipEvents ++ M), lists:sort(UserEvents ++ U)}};
take(_K, [], [], {B, M, U}) ->
    {B, {lists:sort(M), lists:sort(U)}};
take(K, [{T, Event} | Events], UserEvents, {B, M, U}) ->
    take(K - 1, Events, UserEvents, {[{membership, Event} | B], [{T + 1, Event} | M], U});
take(K, [], [{T, Event} | Events], {B, M, U}) ->
    take(K - 1, [], Events, {[{user, Event} | B], M, [{T + 1, Event} | U]}).

-spec prune(Retransmits, Broadcasts0) -> Broadcasts when
      Retransmits :: non_neg_integer(),
      Broadcasts0 :: broadcast(),
      Broadcasts  :: broadcast().

prune(Retransmits, {MembershipEvents0, UserEvents0}) ->
    Filter = fun({T, _}) -> T < Retransmits end,
    MembershipEvents = lists:filter(Filter, MembershipEvents0),
    UserEvents = lists:filter(Filter, UserEvents0),
    {MembershipEvents, UserEvents}.

%% @doc Insert an Event or list of Events to the Broadcast queue
%%
%% Upon inserting a Membership Event we invalidate any existing event about the same target member
%% to prevent the brodcast of stale information.
%% @end
-spec insert(Events, Broadcasts0) -> Broadcasts when
      Events      :: swim:swim_event() | [swim:swim_event()],
      Broadcasts0 :: broadcast(),
      Broadcasts  :: broadcast().

insert(Events, Broadcasts) when is_list(Events) ->
    lists:foldl(fun insert/2, Broadcasts, Events);
insert({membership, Event}, {MembershipEvents, UserEvents}) ->
    {invalidate(Event, MembershipEvents), UserEvents};
insert({user, Event}, {MembershipEvents, UserEvents}) ->
    {MembershipEvents, lists:sort([{0, Event} | UserEvents])}.

-spec invalidate(Event, Events0) -> Events when
      Event   :: swim:membership_event(),
      Events0 :: [swim:membership_event()],
      Events  :: [swim:membership_event()].

invalidate({_, _, Member} = Event, Events) ->
    invalidate(Member, Event, Events);
invalidate({_, _, Member, _} = Event, Events) ->
    invalidate(Member, Event, Events).

-spec invalidate(Member, Event, Events0) -> Events when
      Member  :: swim:member(),
      Event   :: swim:membership_event(),
      Events0 :: [swim:membership_event()],
      Events  :: [swim:membership_event()].

invalidate(Member, Event, Events) ->
    Filter = fun({_, {_, _, M}}) -> M =/= Member;
                ({_, {_, _, M, _}}) -> M =/= Member end,
    lists:sort([{0, Event} | lists:filter(Filter, Events)]).
