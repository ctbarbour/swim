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
-behavior(gen_event).

-export([max_transmissions/2, membership/2, user/2,
         dequeue/2, dequeue/3]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

-include("swim.hrl").

-type member_state() :: {member_status(), member(), incarnation()}.
-type event() :: {member_status(), member(), incarnation()} | term().

-record(state, {
          retransmit_factor = 4  :: pos_integer(),
          membership_events = [] :: [{non_neg_integer(), member_state()}],
          user_events       = [] :: [{non_neg_integer(), binary()}]
         }).

%% @doc Calculates the maximum number of times an event should be broadcast.
-spec max_transmissions(pos_integer(), pos_integer()) -> pos_integer().
max_transmissions(NumMembers, RetransmitFactor) ->
    round(math:log(NumMembers + 1)) * RetransmitFactor.

%% @doc Queues a membership event to be broadcast to other members in the group
-spec membership(pid(), {member_status(), member(), incarnation()}) -> ok.
membership(EventMgrPid, Event) ->
    enqueue(EventMgrPid, membership, Event).

%% @doc Queues a user event to be broadcast to other members in the group
-spec user(pid(), term()) -> ok.
user(EventMgrPid, Event) ->
    enqueue(EventMgrPid, user, Event).

%% {@link dequeue/3}
-spec dequeue(pid(), pos_integer()) -> binary().
dequeue(EventMgrPid, NumMembers) ->
    MaxSize = swim_messages:event_size_limit(),
    dequeue(EventMgrPid, NumMembers, MaxSize).

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
-spec dequeue(pid() | module(), pos_integer(), pos_integer()) -> binary().
dequeue(EventMgrPid, NumMembers, MaxSize) ->
    gen_event:call(EventMgrPid, ?MODULE, {dequeue, MaxSize, NumMembers}).

-spec enqueue(pid() | module(), event_category(), event()) -> ok.
enqueue(EventMgrPid, EventCategory, Event) ->
    gen_event:notify(EventMgrPid, {EventCategory, Event}).

%% @private
init([]) ->
    init([4]);
init([RetransmitFactor]) ->
    {ok, #state{retransmit_factor=RetransmitFactor}}.

%% @private
handle_event({user, Event}, State) ->
    #state{user_events=Events} = State,
    {ok, State#state{user_events=[{0, Event} | Events]}};
handle_event({membership, {_Status, Member, _Inc} = Event}, State) ->
    #state{membership_events=Events} = State,
    FilteredEvents = lists:filter(fun({_, {_, M, _}}) ->
                                          M /= Member
                                  end, Events),
    {ok, State#state{membership_events=[{0, Event}| FilteredEvents]}};
handle_event(_, State) ->
    {ok, State}.

%% @private
handle_call({dequeue, MaxSize, NumMembers}, State) ->
    #state{membership_events=MEvents, retransmit_factor=RetransmitFactor,
           user_events=UEvents} = State,
    MaxTransmissions = max_transmissions(NumMembers, RetransmitFactor),
    {SizeLeft, MBroadcast, MKeep} = do_dequeue(MaxSize, MaxTransmissions,
                                               [{T, {membership, MB}} || {T, MB} <- MEvents]),
    {_, UBroadcast, UKeep} = do_dequeue(SizeLeft, MaxTransmissions,
                                        [{T, {user, UB}} || {T, UB} <- UEvents],
                                        MBroadcast, []),
    Broadcast = lists:foldl(fun(B, Acc) -> << B/binary, Acc/binary>> end, <<>>, UBroadcast),
    {ok, Broadcast, State#state{membership_events=MKeep, user_events=UKeep}};
handle_call(_, State) ->
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
do_dequeue(MaxSize, MaxTransmissions, Events) ->
    SortedEvents = lists:keysort(1, Events),
    do_dequeue(MaxSize, MaxTransmissions, SortedEvents, [], []).

%% @private
do_dequeue(MaxSize, _MaxTransmissions, [], Broadcast, Keep) ->
    {MaxSize, Broadcast, Keep};
do_dequeue(MaxSize, MaxTransmissions, [NextEvent | Events] = E, Broadcast, Keep) ->
    {_Transmissions, Msg} = NextEvent,
    Wire = swim_messages:encode_event(Msg),
    case MaxSize - iolist_size(Wire) of
        NewSize when NewSize >= 0 ->
            NewBroadcast = [Wire | Broadcast],
            do_dequeue(NewSize, MaxTransmissions, Events, NewBroadcast,
                       maybe_keep(MaxTransmissions, NextEvent, Keep));
        NewSize when NewSize < 0 ->
            {0, Broadcast, lists:flatten([[{T, K} || {T, {_, K}} <- E] | Keep])}
    end.

%% @private
maybe_keep(MaxTransmissions, {Transmissions, {_, _Msg}}, Keep)
  when Transmissions + 1 >= MaxTransmissions ->
    Keep;
maybe_keep(MaxTransmissions, {Transmissions, {_, Msg}}, Keep)
  when Transmissions + 1 < MaxTransmissions ->
    [{Transmissions + 1, Msg} | Keep].
