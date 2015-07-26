

# SWIM - An Awesome Weakly-consistent Infection-style Gossip Protocol #

Copyright (c) 2015 Tucker Barbour

__Version:__ Jul 26 2015 10:33:30

__Authors:__ Tucker Barbour ([`barbct5@gmail.com`](mailto:barbct5@gmail.com)).

__References__* http://www.cs.cornell.edu/~asdas/research/dsn02-SWIM.pdf

### Intro

This Application is an Erlang implementation of the
Scalable Weakly-consistent Infection-style Process Group
Membership Protocol (SWIM). As the title implies, SWIM provides
weakly-consistent knowledge of process group membership information to all
participating processes. However, the [*Scalable* part of the title should read:
*Awesome!*](http://erlangcentral.org/scalable-is-awesome-literally-garrett-smith-erlang-user-conference-2015/#.VZWtcXjEo22)
So let's be more specific about what Awesome features SWIM provides:

- Constant message load (bandwidth) per member regardless of the number
of members in the group
- Constant time to first-detection of a faulty process regardless of
the number of members in the group
- Low false-positive failure detection rate

### Use Cases

What can we use SWIM for?

- Reliable multicast
- Epidemic-style information dissemination
- Pub-sub
- Generic peer-to-peer systems

Really anything that requires each process in a group to maintain a local list
of other non-faulty processes in the group and be notified when members join or
leave us to maintain a list of members and detect when, either voluntarily or
through failure.

### Why?

Other distributed membership algorithms tradionally use a heartbeating technique.
Each process periodically sends an incrementing heartbeat counter. Another
process is detected as faulty when a heartbeat is not received from it in some
period of time. However, heartbeat implementations suffer from scalability
limitiations as the size of the process group grows. Some popular
heartbeat implementations:

* Centralized - leads to hot-spots and single-point-of-failure
* All-to-All - leads to message load on the network and group that grows quadratically with the group size
* Logical Ring - unpredicability of failure detection time

SWIM address the problems with tradional heartbeat implementations through a
peer-to-peer randomized probing protocol. I recommend reading the SWIM paper
for more details.

### Why Not?

SWIM provides weak-consistency guarentees of group membership.
If our domain requires stronger consistency membership awareness, then we should look
elsewhere:

- [Zookeeper](https://zookeeper.apache.org)
- [Paxos](http://research.microsoft.com/en-us/um/people/lamport/pubs/paxos-simple.pdf)
- [Raft](https://www.usenix.org/conference/atc14/technical-sessions/presentation/ongaro)
- [Riak Ensemble](https://github.com/basho/riak_ensemble)

What if want more than just membership awareness and fault detection? Say
we want some sort of application-level sharding like a consistent-hash ring?
SWIM and this implemention only provide the features discussed in the SWIM
paper. You can use SWIM as the underlying gossip protocol to disseminate
ring updates to the group -- but that's up to you and your application. You may
be better off taking a look at other, more specific, implementions like:

- [Ringpop](https://github.com/uber/ringpop)
- [Plumtree](https://github.com/helium/plumtree)

What if the information we need to disseminate to the group is large, on
the order of MiB and GiB? In this implementation of SWIM we strictly use UDP for
transport and thus have an upper limit on the size of information we can
reliably send per message. Again, we can use SWIM for membership awareness and write our
application logic using TCP to transmit our large data between members -- up to
you. It might also be worth taking a look at alternative implementations that
have modified the protocol to support both UDP and TCP:

- [Memberlist](https://github.com/hashicorp/memberlist)

### Details

If you made it this far and are still interested, you should read the module
documentation which includes details about the implementation.
The pieces of the SWIM protocol are broken down as follows:

* __*Failure Detection*__ - [`swim_gossip`](swim_gossip.md)
* __*Membership*__ - [`swim_membership`](swim_membership.md)
* __*Dissemination*__ - [`swim_broadcasts`](swim_broadcasts.md)

### How To

Here is a quick reference for using SWIM in your application. These examples
assume the `swim` and `crypto` applications are already started. We also
assume encryption keys have already been distributed -- that`s outside the
scope of SWIM.

```erlang

% On our first node, let us start the seed listening at 192.168.2.10:5000
ok = swim:start_gossip(lan, {192,168,2,10}, 5000, [{keys, Keys}]),

% On a different node, let us join the group "lan" using 192.168.2.10:5000
% as the seed.
Seeds = [{{192,168,2,10}, 5000}],
ok = swim:start_gossip(lan, {192,168,2,11}, 5000, [{seeds, Seeds}, {keys, Keys}]),

% Let us check who else is in the group
swim:members(lan),

% Let us subscribe to membership events. The subscribing process will now begin
% receiving messages when a member joins or leaves the group.
swim:subscribe(lan, membership)

```

### Build

We require OTP-18.x and an OpenSSL that supports AES-GCM. The default on OSX
does not include support for AES-GCM, so it's recommended you use `homebrew` to
install a newer version of OpenSSL and compile OTP linking to the OpenSSL managed
by `homebrew`. Include `--with-ssl=/usr/local/opt/openssl` when compiling OTP.

We use `rebar3`, included in the source of this repo, to build and test `swim`.

```

./rebar3 do xref, dialyzer, eunit

```

## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/barbct5/swim/blob/master/doc/swim.md" class="module">swim</a></td></tr>
<tr><td><a href="https://github.com/barbct5/swim/blob/master/doc/swim_broadcasts.md" class="module">swim_broadcasts</a></td></tr>
<tr><td><a href="https://github.com/barbct5/swim/blob/master/doc/swim_gossip.md" class="module">swim_gossip</a></td></tr>
<tr><td><a href="https://github.com/barbct5/swim/blob/master/doc/swim_gossip_events.md" class="module">swim_gossip_events</a></td></tr>
<tr><td><a href="https://github.com/barbct5/swim/blob/master/doc/swim_membership.md" class="module">swim_membership</a></td></tr>
<tr><td><a href="https://github.com/barbct5/swim/blob/master/doc/swim_messages.md" class="module">swim_messages</a></td></tr>
<tr><td><a href="https://github.com/barbct5/swim/blob/master/doc/swim_vault.md" class="module">swim_vault</a></td></tr>
</table>


