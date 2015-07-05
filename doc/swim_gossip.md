

# Module swim_gossip #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

This module is responsible for the failure detection mechanism of the
SWIM protocol.

Copyright (c) 2015

__Version:__ Jul 21 2015 16:46:31

<a name="description"></a>

## Description ##

When starting the gossip process, we require the Ipv4 address and port number
to send and receive messages from other peers in the gossip group. We can
also provide configuration parameters to tune the underlying protocol.
The parameters we expose are:

+ `{protocol_period, pos_integer()}` : The time, in milliseconds, between
protocol intervals. This value is referred to as __*T*__ in the SWIM paper.
The paper notes that the protocol period has to be at least 3 times the
estimated round-trip time for a message. The protocol period is not
adjustable after the peer is started.

+ `{ack_timeout, pos_integer()}` : The time, in milliseconds, to wait for
an ACK in response to a PING. The ACK timeout must be less than the protocol
period. The ACK timeout should be based on the latency distribution on your
network. Good choices may be the average round-trip time or the 99th
percentile.

+ `{ack_proxies, pos_integer()}` : The number of peers to send indirect
probes via the PING-REQ message. This value is referred to as __*k*__
in the SWIM paper.

+ `{suspicion, pos_integer()}` : A scaling factor for the number of
protocol periods we wait for a suspected member to refute our claim.

+ `{retransmit, pos_integer()}` : A scaling factor for the number of times
we broadcast membership updates to peers in the gossip group. This value
is referred to as __*&#x3bb;*__ in the SWIM paper.

+ `{seeds, [member()]}` : A list of members in an existing gossip group.
When we start the local peer we attempt to join an existing group via the
provided seeds. To start a new gossip group, we can provide 0 seed members.

During each protocol period defined by
`protocol_period`, a random member is selected from the local
peer's membership list and a PING message sent to it. When a peer
is first started the membership list contains the `seeds`. The
local peer then waits for a reply ACK from the target of the
PING. If an ACK is not received within the `ack_timeout` period,
the local peer will indirectly probe the target. The local peer
selects `ack_proxies` members at random, excluding the target of
the original PING, and sends each a PING-REQ message. Each of
these members in turn, on receiving a PING-REQ, sends a PING to
the original PING target and forwards the ACK back to the local
peer. If the proxies do not receive an ACK, the ACK timeout is
ignored and they continue with their business. At the end of the
protcol period the local peer checks if it has received any ACKs
either directly from it's original target or indirectly through
one of the proxies. If not, it declares the target suspicious in it's
local membership list and broadcasts the update to the rest of the
gossip group.

The data contained in each message of the protcol is tagged with
unique sequence number of the protocol period at the
initiator. ACKs will contain the same sequence number contained in
the original PING or PING-REQ. Upon receiving an ACK, the
initiator can then distiguish between ACKs sent in response to a
PING or PING-REQ during the current protocol period or ACKs sent
in response to a PING from a previous protocol period where the
target has already been marked as suspicious. For more information
on the Supicious mechanism, see [`swim_membership`](swim_membership.md).
<a name="types"></a>

## Data Types ##




### <a name="type-gossip_opt">gossip_opt()</a> ###


<pre><code>
gossip_opt() = {protocol_period, pos_integer()} | {ack_timeout, pos_integer()} | {ack_proxies, pos_integer()} | {retransmit, pos_integer()}
</code></pre>




### <a name="type-swim_opt">swim_opt()</a> ###


<pre><code>
swim_opt() = <a href="#type-gossip_opt">gossip_opt()</a> | {keys, [<a href="#type-key">key()</a>]} | <a href="swim_membership.md#type-membership_opt">swim_membership:membership_opt()</a>
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close-1">close/1</a></td><td>Closes the gossip peer without announcing leaving to the rest of
members.</td></tr><tr><td valign="top"><a href="#gossip-2">gossip/2</a></td><td>Gossip an arbitrary term to currently known peers.</td></tr><tr><td valign="top"><a href="#leave-1">leave/1</a></td><td>Closes the gossip peer after announcing leaving to the rest of
members.</td></tr><tr><td valign="top"><a href="#members-1">members/1</a></td><td>Get all the members known to the gossip peer including itself.</td></tr><tr><td valign="top"><a href="#members_with_status-1">members_with_status/1</a></td><td>Get all the members known to the gossip peer including itself along with
the locally observed status of the member.</td></tr><tr><td valign="top"><a href="#rotate_keys-2">rotate_keys/2</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-4">start_link/4</a></td><td>Starts the Gossip process for this Node.</td></tr><tr><td valign="top"><a href="#stats-1">stats/1</a></td><td>Retreive protocol stats.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="close-1"></a>

### close/1 ###

<pre><code>
close(Name::atom()) -&gt; ok
</code></pre>
<br />

Closes the gossip peer without announcing leaving to the rest of
members.

<a name="gossip-2"></a>

### gossip/2 ###

<pre><code>
gossip(Name::atom(), Event::term()) -&gt; ok
</code></pre>
<br />

Gossip an arbitrary term to currently known peers.

Gossiping a term to the rest of the peers piggybacks on the same protocol
used for disseminating membership changes. Terms provided are subject to size
limitations defined in [`swim_messages:event_size_limit/0`](swim_messages.md#event_size_limit-0).

<a name="leave-1"></a>

### leave/1 ###

<pre><code>
leave(Name::atom()) -&gt; ok
</code></pre>
<br />

Closes the gossip peer after announcing leaving to the rest of
members.

<a name="members-1"></a>

### members/1 ###

<pre><code>
members(Name::atom()) -&gt; [<a href="#type-member">member()</a>]
</code></pre>
<br />

Get all the members known to the gossip peer including itself. Members
include both alive and suspicious members.

<a name="members_with_status-1"></a>

### members_with_status/1 ###

<pre><code>
members_with_status(Name::atom()) -&gt; [{<a href="#type-member">member()</a>, <a href="#type-member_status">member_status()</a>}]
</code></pre>
<br />

Get all the members known to the gossip peer including itself along with
the locally observed status of the member.

<a name="rotate_keys-2"></a>

### rotate_keys/2 ###

<pre><code>
rotate_keys(Name::atom(), Key::<a href="#type-key">key()</a>) -&gt; ok
</code></pre>
<br />

<a name="start_link-4"></a>

### start_link/4 ###

<pre><code>
start_link(Name::atom(), ListenIp::<a href="inet.md#type-ip_address">inet:ip_address()</a>, ListenPort::<a href="inet.md#type-port_number">inet:port_number()</a>, Opts::[<a href="#type-gossip_opt">gossip_opt()</a>]) -&gt; {ok, pid()} | {error, term()}
</code></pre>
<br />

Starts the Gossip process for this Node.

<a name="stats-1"></a>

### stats/1 ###

<pre><code>
stats(Name::atom()) -&gt; list()
</code></pre>
<br />

Retreive protocol stats. Stats included [`inet:getstat/1`](inet.md#getstat-1).

