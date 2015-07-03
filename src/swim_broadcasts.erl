%%% ----------------------------------------------------------------------------
%%% Copyright (c) 2015. All Rights Reserved.
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

%%% @copyright 2015
%%% @version {@version}

%%% @doc This module is responsible for maintaining membership updates and user
%%% provided events along with their state as a part of the
%%% infection-style dissemination component of the SWIM protocol.
%%%
%%% == Infection-Style Dissemination ==
%%% As an alternative to IP Multicast
%%% or a point-to-point messaging scheme the SWIM protocol
%%% disseminates membership updates by piggybacking on messages sent
%%% as a part of the failure detection protcool. The implementation
%%% does not generate any extra packets.
%%%
%%% Here, `swim_broadcasts' maintains the buffer of recent membership
%%% updates along with a count for each event. The local count
%%% specifies the number of times the event has been piggybacked so
%%% far by this member and is used to choose which events to piggyback
%%% next. Each event is piggybacked at most <code>Retransmit * log(N +
%%% 1)</code> times. If the size of events in the buffer is larger
%%% than the maximum number of events that can be piggybacked on a
%%% single PING or ACK, events that bave been gossiped fewer times are
%%% preferred. See {@link swim_messages:event_size_limit/0} for more
%%% information on how this limit is derived. This is needed as the
%%% protocol period is fixed and the rater of membership changes might
%%% temporarily overwhelm the speed of dissemination. Preferring
%%% "younger" events under such circumstances ensures that tall
%%% membership changes infect at least a few members - when the
%%% membership change inject rate quiesces, these changes will
%%% propagate through the rest of the gossip group.
%%%
%%% @end
-module(swim_broadcasts).

-export([new/1,
	 pop/4,
	 peek/1,
	 push/2]).

-record(?MODULE, {
	   retransmit    :: pos_integer(),
	   broadcasts    :: [broadcast()]
	  }).

-include("swim.hrl").

-type piggybacks()        :: non_neg_integer().
-type broadcast()         :: {swim_event(), piggybacks()}.
-type encode()            :: fun((swim_event()) -> binary()).
-opaque swim_broadcast()  :: #?MODULE{}.

-export_type([swim_broadcast/0]).

%% @doc Creates a new Swim Broadcasts. Retransmit is a scaling factor for the
%% number of times an event is broadcast to ensure quick and complete
%% dissemination. See {@link pop/4} on how Retransmit is used.
-spec new(pos_integer()) -> swim_broadcast().
new(Retransmit) when is_integer(Retransmit), Retransmit > 0 ->
    #?MODULE{retransmit=Retransmit, broadcasts=[]}.

%% @doc Adds a Swim Event to be broadcast.
-spec push(swim_event(), swim_broadcast()) -> swim_broadcast().
push(Event, State) ->
    #?MODULE{broadcasts=Broadcasts} = State,
    State#?MODULE{broadcasts=[{Event, 0} | Broadcasts]}.

%% @doc Peek at all Swim Events waiting to be broadcast.
-spec peek(swim_broadcast()) -> [swim_event()].
peek(#?MODULE{broadcasts=Broadcasts}) ->
    lists:map(fun({Event, _P}) -> Event end,
		  lists:sort(fun sort/2, Broadcasts)).

%% @doc Returns encoded Swim Events to be broadcast to the rest of the peers.
%%
%% Events to be broadcast are determined by the number of the peers as well as
%% the size limitation provided by SizeLimit. Membership events always take
%% precedence over user events. Events are broadcast up to a max of
%% <code>Retransmit * log(NumMembers + 1)</code>. If the number of events
%% exceeds the maximum number of events dictated by SizeLimit, events that have
%% been broadcast fewer times are preferred. This is needed as the rate of
%% incoming events, i.e. membership changes, might temporarily overwhelm
%% the speed of dissemination.
%% Preferring younger events ensures that all events
%% infect at least a few members. Events that have exceeded
%% their retransmit limit are removed from the broadcasts. Events that are
%% returned have their number of retransmissions incremented by 1.
%% @end
-spec pop(pos_integer(), pos_integer(), encode(), swim_broadcast()) ->
		 {[binary()], swim_broadcast()}.
pop(SizeLimit, NumMembers, Encode, State) ->
    #?MODULE{broadcasts=Broadcasts, retransmit=Retransmit} = State,
    MaxPiggyback = round(Retransmit * math:log(NumMembers + 1)),
    SortedBroadcasts = lists:sort(fun sort/2, Broadcasts),
    {Issued, Remaining} = take_max_events(SizeLimit, MaxPiggyback,
					  Encode, SortedBroadcasts),
    {Issued, State#?MODULE{broadcasts=Remaining}}.

-spec sort(broadcast(), broadcast()) -> boolean().
sort({{Type, _}, P1}, {{Type, _}, P2}) ->
    P1 < P2;
sort({{user, _}, _P1}, {{membership, _}, _P2}) ->
    true;
sort({{membership, _}, _P1}, {{user, _}, _P2}) ->
    false.

-spec take_max_events(pos_integer(), pos_integer(), encode(), [broadcast()]) ->
			     {[binary()], [broadcast()]}.
take_max_events(SizeLimit, MaxPiggybacks, Encode, Broadcasts) ->
    Acc = {[], []},
    take_max_events(Broadcasts, Acc, SizeLimit, Encode, SizeLimit, MaxPiggybacks).

-spec take_max_events([broadcast()], {[binary()], [broadcast()]}, pos_integer(),
		      encode(), pos_integer(), pos_integer())
		     -> {[binary()], [broadcast()]}.
take_max_events([], Acc, _, _, _, _) ->
    Acc;
take_max_events([{Event, _P} = B | Broadcasts], {Encoded, Remaining}, Size, Encode, MaxEventSize, MaxPiggybacks) ->
    Bin = Encode(Event),
    BinSize = size(Bin),
    case {Size - BinSize, BinSize} of
	{SizeLeft, _BinSize} when SizeLeft >= 0 ->
	    NewEncoded = [Bin | Encoded],
	    NewRemaining = maybe_keep(B, MaxPiggybacks, Remaining),
	    take_max_events(Broadcasts, {NewEncoded, NewRemaining},
			    SizeLeft,
			    Encode,
			    MaxEventSize,
			    MaxPiggybacks);
	{_SizeLeft, BinSize} when BinSize > MaxEventSize ->
	    ok = error_logger:warning_msg("Event too large. ~p\n", [Event]),
	    take_max_events(Broadcasts, {Encoded, Remaining}, Size,
			    Encode,
			    MaxEventSize,
			    MaxPiggybacks);
	_ ->
	    {Encoded, Broadcasts ++ Remaining}
    end.

-spec maybe_keep(broadcast(), pos_integer(), [broadcast()]) -> [broadcast()].
maybe_keep({Event, Piggybacks}, MaxPiggybacks, Remaining) ->
    case Piggybacks < MaxPiggybacks of
	true ->
	    [{Event, Piggybacks + 1} | Remaining];
	false ->
	    Remaining
    end.
