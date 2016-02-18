

# Module swim_broadcasts #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

This module is responsible for maintaining membership updates and user
provided events along with their state as a part of the
infection-style dissemination component of the SWIM protocol.

Copyright (c) 2015

__Version:__ Feb 18 2016 10:13:03

<a name="description"></a>

## Description ##

### Infection-Style Dissemination
As an alternative to IP Multicast
or a point-to-point messaging scheme the SWIM protocol
disseminates membership updates by piggybacking on messages sent
as a part of the failure detection protocol. Thus, implementation
does not generate any extra packets to send membership updates.

Here, `swim_broadcasts` maintains the buffer of recent membership
events along with a count for each event. The local count
specifies the number of times the event has been piggybacked so
far by this member and is used to choose which events to piggyback
next. Each event is piggybacked at most `Retransmit * log(N +
1)` times, where `Retransmit` is a configurable parameter.
If the size of events in the buffer is larger than the maximum number of
events that can be piggybacked on a single PING or ACK, events that have
been gossiped fewer times are preferred. This is needed as the
protocol period is fixed and the rate of membership changes might
temporarily overwhelm the speed of dissemination. Preferring
"younger" events under such circumstances ensures that all
membership changes infect at least a few members - when the
membership change rate quiesces, older events will
propagate through the rest of the gossip group. Membership events are always
preferred over user-provided events.
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#dequeue-2">dequeue/2</a></td><td></td></tr><tr><td valign="top"><a href="#dequeue-3">dequeue/3</a></td><td>Dequeues a set of encoded events ready to be broadcast to other members
in the group.</td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#max_transmissions-2">max_transmissions/2</a></td><td>Calculates the maximum number of times an event should be broadcast.</td></tr><tr><td valign="top"><a href="#membership-2">membership/2</a></td><td>Queues a membership event to be broadcast to other members in the group.</td></tr><tr><td valign="top"><a href="#user-2">user/2</a></td><td>Queues a user event to be broadcast to other members in the group.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="dequeue-2"></a>

### dequeue/2 ###

<pre><code>
dequeue(EventMgrPid::pid(), NumMembers::pos_integer()) -&gt; binary()
</code></pre>
<br />

<a name="dequeue-3"></a>

### dequeue/3 ###

<pre><code>
dequeue(EventMgrPid::pid() | module(), NumMembers::pos_integer(), MaxSize::pos_integer()) -&gt; binary()
</code></pre>
<br />

Dequeues a set of encoded events ready to be broadcast to other members
in the group

Events to be broadcast are determined by the number of the peers as well as
the size limitation provided by `MaxSize`. Membership events always take
precedence over user events. Events are broadcast up to a max of
determined by [`max_tranmissions/2`](#max_tranmissions-2). If the number of events
exceeds the maximum number of events allowable under `MaxSize`, events that have
been broadcast fewer times are preferred. This is needed as the rate of
incoming events, i.e. membership changes, might temporarily overwhelm
the speed of dissemination.
Preferring younger events ensures that all events
infect at least a few members. Events that have exceeded
their retransmit limit are removed from the broadcasts. Events that are
returned have their number of retransmissions incremented by 1.

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Info, State) -> any()`

<a name="max_transmissions-2"></a>

### max_transmissions/2 ###

<pre><code>
max_transmissions(NumMembers::pos_integer(), RetransmitFactor::pos_integer()) -&gt; pos_integer()
</code></pre>
<br />

Calculates the maximum number of times an event should be broadcast.

<a name="membership-2"></a>

### membership/2 ###

<pre><code>
membership(EventMgrPid::pid(), Event::{<a href="#type-member_status">member_status()</a>, <a href="#type-member">member()</a>, <a href="#type-incarnation">incarnation()</a>}) -&gt; ok
</code></pre>
<br />

Queues a membership event to be broadcast to other members in the group

<a name="user-2"></a>

### user/2 ###

<pre><code>
user(EventMgrPid::pid(), Event::term()) -&gt; ok
</code></pre>
<br />

Queues a user event to be broadcast to other members in the group

