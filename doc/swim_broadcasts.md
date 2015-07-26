

# Module swim_broadcasts #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

This module is responsible for maintaining membership updates and user
provided events along with their state as a part of the
infection-style dissemination component of the SWIM protocol.

Copyright (c) 2015

__Version:__ Jul 26 2015 10:56:59

<a name="description"></a>

## Description ##

### Infection-Style Dissemination
As an alternative to IP Multicast
or a point-to-point messaging scheme the SWIM protocol
disseminates membership updates by piggybacking on messages sent
as a part of the failure detection protocol. Thus, implementation
does not generate any extra packets to send membership updates.

Here, `swim_broadcasts` maintains the buffer of recent membership
updates along with a count for each event. The local count
specifies the number of times the event has been piggybacked so
far by this member and is used to choose which events to piggyback
next. Each event is piggybacked at most `Retransmit * log(N +
1)` times. If the size of events in the buffer is larger
than the maximum number of events that can be piggybacked on a
single PING or ACK, events that have been gossiped fewer times are
preferred. See [`swim_messages:event_size_limit/0`](swim_messages.md#event_size_limit-0) for more
information on how this limit is derived. This is needed as the
protocol period is fixed and the rate of membership changes might
temporarily overwhelm the speed of dissemination. Preferring
"younger" events under such circumstances ensures that all
membership changes infect at least a few members - when the
membership change rate quiesces, older events will
propagate through the rest of the gossip group.

<a name="types"></a>

## Data Types ##




### <a name="type-encode">encode()</a> ###


<pre><code>
encode() = fun((<a href="#type-swim_event">swim_event()</a>) -&gt; binary())
</code></pre>




### <a name="type-swim_broadcast">swim_broadcast()</a> ###


__abstract datatype__: `swim_broadcast()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Creates a new Swim Broadcasts.</td></tr><tr><td valign="top"><a href="#peek-1">peek/1</a></td><td>Peek at all Swim Events waiting to be broadcast.</td></tr><tr><td valign="top"><a href="#pop-4">pop/4</a></td><td>Returns encoded Swim Events to be broadcast to the rest of the peers.</td></tr><tr><td valign="top"><a href="#push-2">push/2</a></td><td>Adds a Swim Event to be broadcast.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Retransmit::pos_integer()) -&gt; <a href="#type-swim_broadcast">swim_broadcast()</a>
</code></pre>
<br />

Creates a new Swim Broadcasts. Retransmit is a scaling factor for the
number of times an event is broadcast to ensure quick and complete
dissemination. See [`pop/4`](#pop-4) on how Retransmit is used.

<a name="peek-1"></a>

### peek/1 ###

<pre><code>
peek(State::<a href="#type-swim_broadcast">swim_broadcast()</a>) -&gt; [<a href="#type-swim_event">swim_event()</a>]
</code></pre>
<br />

Peek at all Swim Events waiting to be broadcast.

<a name="pop-4"></a>

### pop/4 ###

<pre><code>
pop(SizeLimit::pos_integer(), NumMembers::pos_integer(), Encode::<a href="#type-encode">encode()</a>, State::<a href="#type-swim_broadcast">swim_broadcast()</a>) -&gt; {[binary()], <a href="#type-swim_broadcast">swim_broadcast()</a>}
</code></pre>
<br />

Returns encoded Swim Events to be broadcast to the rest of the peers.

Events to be broadcast are determined by the number of the peers as well as
the size limitation provided by SizeLimit. Membership events always take
precedence over user events. Events are broadcast up to a max of
`Retransmit * log(NumMembers + 1)`. If the number of events
exceeds the maximum number of events dictated by SizeLimit, events that have
been broadcast fewer times are preferred. This is needed as the rate of
incoming events, i.e. membership changes, might temporarily overwhelm
the speed of dissemination.
Preferring younger events ensures that all events
infect at least a few members. Events that have exceeded
their retransmit limit are removed from the broadcasts. Events that are
returned have their number of retransmissions incremented by 1.

<a name="push-2"></a>

### push/2 ###

<pre><code>
push(Event::<a href="#type-swim_event">swim_event()</a>, State::<a href="#type-swim_broadcast">swim_broadcast()</a>) -&gt; <a href="#type-swim_broadcast">swim_broadcast()</a>
</code></pre>
<br />

Adds a Swim Event to be broadcast.

