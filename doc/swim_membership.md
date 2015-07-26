

# Module swim_membership #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

This module is responsible for maintaining the list and status of non
faulty members in a gossip group through the use of the Suspicion Mechanism
described in the SWIM Paper.

Copyright (c) 2015

__Version:__ Jul 26 2015 10:56:59

<a name="description"></a>

## Description ##
A `swim_membership` is a data structure. It
should be updated when new members join the gossip group
or drop out as determined by the Failure Detection mechanism of SWIM
implemented as [`swim_gossip`](swim_gossip.md). A `swim_membership`
further maintains the local gossip peer status, which includes the current
incarnation -- the logical clock for the member status. When a
`swim_membership` is updated via [`swim_membership:update/3`](swim_membership.md#update-3) a list of
membership events is returned indicating changes to the local membership list
which should be broadcast to the rest of the members in the gossip group.

<a name="types"></a>

## Data Types ##




### <a name="type-membership">membership()</a> ###


__abstract datatype__: `membership()`




### <a name="type-membership_opt">membership_opt()</a> ###


<pre><code>
membership_opt() = {suspicion, pos_integer()} | {seeds, [<a href="#type-member">member()</a>]}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#age_members-1">age_members/1</a></td><td>Increment the age of Members in the Membership list.</td></tr><tr><td valign="top"><a href="#alive-2">alive/2</a></td><td>Update Member's status to Alive using the most recently observed
incarnation.</td></tr><tr><td valign="top"><a href="#faulty_members-1">faulty_members/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_member-2">is_member/2</a></td><td>Returns a boolean indicating whether the Member is in the Membership
list or not.</td></tr><tr><td valign="top"><a href="#leave-2">leave/2</a></td><td>Update Membership to indicate the Member has willingly left.</td></tr><tr><td valign="top"><a href="#members-1">members/1</a></td><td>Returns a list of Members -- both alive or suspects.</td></tr><tr><td valign="top"><a href="#members_with_status-1">members_with_status/1</a></td><td>Returns the list of Members with their current status.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Creates a new Membership list.</td></tr><tr><td valign="top"><a href="#size-1">size/1</a></td><td>Returns the number of Members in the Membership list.</td></tr><tr><td valign="top"><a href="#status-2">status/2</a></td><td>Returns the current status of a Member in the local Membership list.</td></tr><tr><td valign="top"><a href="#suspect-2">suspect/2</a></td><td>Suspect a Member.</td></tr><tr><td valign="top"><a href="#update-3">update/3</a></td><td>Update a Member's status using the currently known
incarnation.</td></tr><tr><td valign="top"><a href="#update-4">update/4</a></td><td>Update a Member's status using a remote incarnation.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="age_members-1"></a>

### age_members/1 ###

<pre><code>
age_members(Mbrs::<a href="#type-membership">membership()</a>) -&gt; {[<a href="#type-membership_event">membership_event()</a>], <a href="#type-membership">membership()</a>}
</code></pre>
<br />

Increment the age of Members in the Membership list. The
gossip protocol should age it's Members on each protocol period to
age out suspects.

We keep track Member's age as a useful piece of information to
track the churn of Membership. It's also important to track the age
of Member's so we can easily age out suspects and mark them as
faulty after a number of protcol periods have passed. We use the
suspicion multiplier provided on initialization of the
Membership. We age out suspects after `round(Suspicion *
math:log(NumMembers + 1)` protocol periods have elapsed.

<a name="alive-2"></a>

### alive/2 ###

<pre><code>
alive(Member::<a href="#type-member">member()</a>, Mbrs::<a href="#type-membership">membership()</a>) -&gt; {[<a href="#type-membership_event">membership_event()</a>], <a href="#type-membership">membership()</a>}
</code></pre>
<br />

Update Member's status to Alive using the most recently observed
incarnation.

<a name="faulty_members-1"></a>

### faulty_members/1 ###

<pre><code>
faulty_members(Mbrs::<a href="#type-membership">membership()</a>) -&gt; [<a href="#type-member">member()</a>]
</code></pre>
<br />

<a name="is_member-2"></a>

### is_member/2 ###

<pre><code>
is_member(Member::<a href="#type-member">member()</a>, Mbrs::<a href="#type-membership">membership()</a>) -&gt; boolean()
</code></pre>
<br />

Returns a boolean indicating whether the Member is in the Membership
list or not

<a name="leave-2"></a>

### leave/2 ###

<pre><code>
leave(Member::<a href="#type-member">member()</a>, Mbrs::<a href="#type-membership">membership()</a>) -&gt; {[<a href="#type-membership_event">membership_event()</a>], <a href="#type-membership">membership()</a>}
</code></pre>
<br />

Update Membership to indicate the Member has willingly left.

<a name="members-1"></a>

### members/1 ###

<pre><code>
members(Mbrs::<a href="#type-membership">membership()</a>) -&gt; [<a href="#type-member">member()</a>]
</code></pre>
<br />

Returns a list of Members -- both alive or suspects.

<a name="members_with_status-1"></a>

### members_with_status/1 ###

<pre><code>
members_with_status(Mbrs::<a href="#type-membership">membership()</a>) -&gt; [{<a href="#type-member">member()</a>, <a href="#type-member_status">member_status()</a>}]
</code></pre>
<br />

Returns the list of Members with their current status

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Me::<a href="#type-member">member()</a>, Opts::[<a href="#type-membership_opt">membership_opt()</a>]) -&gt; <a href="#type-membership">membership()</a>
</code></pre>
<br />

Creates a new Membership list.

The Membership list requires the owning Member of the Membership
list in order to refute any contradictory membership events targeting the
owner. We can also provide Members to to populate the Membership list at
initalization. Further, we can provide configuration for the suspicion
interval.

<a name="size-1"></a>

### size/1 ###

<pre><code>
size(Mbrs::<a href="#type-membership">membership()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

Returns the number of Members in the Membership list. Includes Members
who are either alive or suspected.

<a name="status-2"></a>

### status/2 ###

<pre><code>
status(Member::<a href="#type-member">member()</a>, Mbrs::<a href="#type-membership">membership()</a>) -&gt; {ok, <a href="#type-member_status">member_status()</a>} | {error, not_found}
</code></pre>
<br />

Returns the current status of a Member in the local Membership list

<a name="suspect-2"></a>

### suspect/2 ###

<pre><code>
suspect(Member::<a href="#type-member">member()</a>, Mbrs::<a href="#type-membership">membership()</a>) -&gt; {[<a href="#type-membership_event">membership_event()</a>], <a href="#type-membership">membership()</a>}
</code></pre>
<br />

Suspect a Member

Suspecting a Member updates the status of the Member and resets it's age.

<a name="update-3"></a>

### update/3 ###

<pre><code>
update(Member::<a href="#type-member">member()</a>, Status::<a href="#type-member_status">member_status()</a>, Mbrs::<a href="#type-membership">membership()</a>) -&gt; {[<a href="#type-membership_event">membership_event()</a>], <a href="#type-membership">membership()</a>}
</code></pre>
<br />

Update a Member's status using the currently known
incarnation.

Updating a Member's status using the currently known incarnation is
used when we observer a change in a Member's status and would like
to broadcast any side-effects to other Members.

<a name="update-4"></a>

### update/4 ###

<pre><code>
update(Member::<a href="#type-member">member()</a>, Status::<a href="#type-member_status">member_status()</a>, Incarnation::<a href="#type-incarnation">incarnation()</a>, Mbrs::<a href="#type-membership">membership()</a>) -&gt; {[<a href="#type-membership_event">membership_event()</a>], <a href="#type-membership">membership()</a>}
</code></pre>
<br />

Update a Member's status using a remote incarnation.

Updating a Member's status using a remote incarnation is used to
override known Membership based on remote observations of a
Member's status.

