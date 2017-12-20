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
%%% Here, `swim_broadcasts' maintains the buffer of recent membership
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
%%% @end
-module(swim_broadcasts).

-export([new/1]).
-export([insert/2]).
-export([append/2]).
-export([take/2]).
-export([take/3]).
-export([retransmit_limit/2]).

-type queue(E) :: [{non_neg_integer(), E}].
-type broadcast() :: {non_neg_integer(), queue(swim:membership_event()), queue(swim:user_event())}.
-export_type([broadcast/0]).

new(RetransmitFactor) ->
    {RetransmitFactor, [], []}.

retransmit_limit(NumMembers, {RetransmitFactor, _, _}) ->
    round(math:log(NumMembers + 1)) + RetransmitFactor.

%% @doc Dequeues a set of encoded events ready to be broadcast to other members
%% in the group
%%
%% Events to be broadcast are determined by the number of the peers as well as
%% the size limitation provided by `MaxSize'. Membership events always take
%% precedence over user events. Events are broadcast up to a max of
%% determined by {@link max_tranmissions/2}. If the number of events
%% exceeds the maximum number of events allowable under `MaxSize', events that have
%% been broadcast fewer times are preferred. This is needed as the rate of
%% incoming events, i.e. membership changes, might temporarily overwhelm
%% the speed of dissemination.
%% Preferring younger events ensures that all events
%% infect at least a few members. Events that have exceeded
%% their retransmit limit are removed from the broadcasts. Events that are
%% returned have their number of retransmissions incremented by 1.
%% @end

take(Retransmits, {R, [{T, Event} | MembershipEvents0], UserEvents}) ->
    MembershipEvents = maybe_keep(Retransmits, {T, Event}, MembershipEvents0),
    {{membership, Event}, {R, MembershipEvents, UserEvents}};
take(Retransmits, {R, [], [{T, Event} | UserEvents0]}) ->
    UserEvents = maybe_keep(Retransmits, {T, Event}, UserEvents0),
    {{user, Event}, {R, [], UserEvents}};
take(_Retransmits, {_R, [], []}) ->
    empty.

take(Max, Retransmits, Broadcasts) ->
    lists:foldl(
      fun(_, {A, B0}) ->
              case take(Retransmits, B0) of
                  {E, B1} ->
                      {[E | A], B1};
                  empty ->
                      {A, B0}
              end
      end, {[], Broadcasts}, lists:seq(1, Max)).

maybe_keep(Retransmits, {T0, Event}, Events0) ->
    case T0 + 1 of
        T when T >= Retransmits -> Events0;
        T -> lists:sort([{T, Event} | Events0])
    end.

append(Events, Broadcasts) when is_list(Events) ->
    lists:foldl(fun insert/2, Broadcasts, Events).

insert({membership, Event}, {R, MembershipEvents, UserEvents}) ->
    {R, invalidate(Event, MembershipEvents), UserEvents};
insert({user, Event}, {R, MembershipEvents, UserEvents}) ->
    {R, MembershipEvents, [{0, Event} | UserEvents]};
insert(Event, Broadcasts) ->
    insert({membership, Event}, Broadcasts).

-spec invalidate(Event, Events0) -> Events when
      Event   :: swim:membership_event(),
      Events0 :: [swim:membership_event()],
      Events  :: [swim:membership_event()].

invalidate({_, _, Member} = Event, Events) ->
    invalidate(Member, Event, Events);
invalidate({_, _, Member, _} = Event, Events) ->
    invalidate(Member, Event, Events).

invalidate(Member, Event, Events) ->
    Filter = fun({_, {_, _, M}}) -> M =/= Member;
                ({_, {_, _, M, _}}) -> M =/= Member end,
    [{0, Event} | lists:filter(Filter, Events)].
