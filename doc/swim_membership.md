

# Module swim_membership #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

This module is responsible for maintaining the list and status of non
faulty members in a gossip group through the use of the Suspicion Mechanism
described in the SWIM Paper.

Copyright (c) 2015

__Version:__ Feb 18 2016 00:10:58

<a name="description"></a>

## Description ##
A `swim_membership` process becomes aware of
membership changes through exported function defined for a specific member
status, [`alive/3`](#alive-3), [`suspect/3`](#suspect-3), [`faulty/3`](#faulty-3), as determined
by the Failure Detection mechanism of SWIM implemented in [`swim`](swim.md). Member
state includes the locally known status of a member as well as a logical clock
for the member's status known in the SWIM paper as the incarnation.
When the status of a member changes events are sent to [`swim_broadcast`](swim_broadcast.md)
to be broadcast to the rest of the members in the gossip group.

<a name="types"></a>

## Data Types ##




### <a name="type-swim_membership_opt">swim_membership_opt()</a> ###


<pre><code>
swim_membership_opt() = {seeds, [<a href="#type-member">member()</a>]} | {suspicion_factor, pos_integer()} | {protocol_period, pos_integer()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#alive-3">alive/3</a></td><td>Set the member status to alive.</td></tr><tr><td valign="top"><a href="#faulty-3">faulty/3</a></td><td>Remove the member from the group.</td></tr><tr><td valign="top"><a href="#local_member-1">local_member/1</a></td><td>The identifier for the local member.</td></tr><tr><td valign="top"><a href="#members-1">members/1</a></td><td>A list of known members and their status.</td></tr><tr><td valign="top"><a href="#num_members-1">num_members/1</a></td><td>The number of known members in the gossip group, including the local member.</td></tr><tr><td valign="top"><a href="#set_status-3">set_status/3</a></td><td>Forcibly set the status of a member.</td></tr><tr><td valign="top"><a href="#start_link-3">start_link/3</a></td><td></td></tr><tr><td valign="top"><a href="#suspect-3">suspect/3</a></td><td>Set the member status to suspect.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="alive-3"></a>

### alive/3 ###

<pre><code>
alive(Pid::pid(), Member::<a href="#type-member">member()</a>, Incarnation::<a href="#type-incarnation">incarnation()</a>) -&gt; [{<a href="#type-member_status">member_status()</a>, <a href="#type-member">member()</a>, <a href="#type-incarnation">incarnation()</a>}]
</code></pre>
<br />

Set the member status to alive

If the member isn't known it's added to the membership and an event is
broadcast to the group. If the member is known and the incarnation is
greater than the current incarnation of the member, we update the incarnation
of member and broadcast an event to group. Otherwise, we do nothing.

<a name="faulty-3"></a>

### faulty/3 ###

<pre><code>
faulty(Pid::pid(), Member::<a href="#type-member">member()</a>, Incarnation::<a href="#type-incarnation">incarnation()</a>) -&gt; [{<a href="#type-member_status">member_status()</a>, <a href="#type-member">member()</a>, <a href="#type-incarnation">incarnation()</a>}]
</code></pre>
<br />

Remove the member from the group

If the member isn't already known we do nothing. If the member is known
we remove the member and broadcast the change if the provided incarnation is
greater than the current incarnation of the member.

<a name="local_member-1"></a>

### local_member/1 ###

<pre><code>
local_member(Pid::pid()) -&gt; <a href="#type-member">member()</a>
</code></pre>
<br />

The identifier for the local member

<a name="members-1"></a>

### members/1 ###

<pre><code>
members(Pid::pid()) -&gt; [{<a href="#type-member_status">member_status()</a>, <a href="#type-member">member()</a>, <a href="#type-incarnation">incarnation()</a>}]
</code></pre>
<br />

A list of known members and their status

<a name="num_members-1"></a>

### num_members/1 ###

<pre><code>
num_members(Pid::pid()) -&gt; pos_integer()
</code></pre>
<br />

The number of known members in the gossip group, including the local member

<a name="set_status-3"></a>

### set_status/3 ###

<pre><code>
set_status(Pid::pid(), Member::<a href="#type-member">member()</a>, Status::<a href="#type-member_status">member_status()</a>) -&gt; ok
</code></pre>
<br />

Forcibly set the status of a member

In certain circumstances we want to be able to set the status of a member
without regard to the rules of the suspecicon mechanism which uses a member's
current status and incarnation.

<a name="start_link-3"></a>

### start_link/3 ###

<pre><code>
start_link(LocalMember::<a href="#type-member">member()</a>, EventMgrPid::pid(), Opts::[<a href="#type-swim_membership_opt">swim_membership_opt()</a>]) -&gt; {ok, pid()}
</code></pre>
<br />

<a name="suspect-3"></a>

### suspect/3 ###

<pre><code>
suspect(Pid::pid(), Member::<a href="#type-member">member()</a>, Incarnation::<a href="#type-incarnation">incarnation()</a>) -&gt; [{<a href="#type-member_status">member_status()</a>, <a href="#type-member">member()</a>, <a href="#type-incarnation">incarnation()</a>}]
</code></pre>
<br />

Set the member status to suspect

If the member isn't already known we do nothing. If the member is known
we update the status and broadcast the change on the follow conditions. If
the current status of the member is alive and the incarnation is greater than
or equal to the known incarnation of the member, we update the member's status
to suspect and broadcast the change. If the current status of the member is
suspect and the incarnation is greater than the known incarnation, we update
the member's status to suspect, set the known incarnation to the provided
incarnation and broadcast the change.
If the suspected member is the local member we refute by incrementing our own
incarnation and broadcasting the change to the group.

