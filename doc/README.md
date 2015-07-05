

# SWIM - An Awesome Weakly-consistent Infection-style Gossip Protocol #

Copyright (c) 2015 Tucker Barbour

__Version:__ Jul 21 2015 16:46:31

__Authors:__ Tucker Barbour ([`barbct5@gmail.com`](mailto:barbct5@gmail.com)).

__References__* http://www.cs.cornell.edu/~asdas/research/dsn02-SWIM.pdf

### Intro

This Application is an Erlang implementation of the
__*S*__calable __*W*__eakly-consistent __*I*__nfection-style Process Group
__*M*__embership Protocol (SWIM). As the title implies, SWIM provides
weakly-consistent knowledge of process group membership information to all
participating processes. However, the [*Scalable* part of the title should read:
*Awesome!*](http://erlangcentral.org/scalable-is-awesome-literally-garrett-smith-erlang-user-conference-2015/#.VZWtcXjEo22)
So let's be more specific about what Awesome features SWIM provides:

- Constant message load (bandwidth) per member regardless of the number
of members in the group
- Constant time to first-detection of a faulty process regardless of
the number of members in the group
- A deterministic time bound to detect failures
- Low false-positive failure detection rate

### Use Cases

What can we use SWIM for?

- Reliable multicast
- Epidemic-style information dissemination
- Pub-sub
- Generic peer-to-peer systems

Really anything that requires us to maintain a list of members and detect when
members join or leave the group, either voluntarily or through failure.

### Why?

You may be thinking, "Why do we need the additional complexity of SWIM just to
maintain a list of processes?" A popular approach you are probably already
familiar with is to periodically send a message (heartbeat) to all the
processes we know about and wait for a response. If we don't receive a response
within some amount of time then we can call the process dead, either no longer
running or unable to service requests. This has it's limitations as the number
of members in the group increase. Sending heartbeats to all members leads to
message load on the network that grows quadratically with the group size. If
your group size is guaranteed to remain small, then this is a perfectly
suitable solution. But what if our domain requires groups of processes in the
order of thousands? Rather than sending heartbeats all other members what if we
instead send our heartbeats along a logical ring of the group members? This
approach may solve the message load problem but still has it's disadvantages. We
still suffer from unpredictable failure detection time when there are multiple
failures. Multiple failures may seem unlikely but as the size of the group grows
so does the probability of failure. It would be nice if we had a protocol that
provided constant message load and a predictable time-to-failure-detection as
the size of the group grows. Oh wait, we do. SWIM.

### Why Not?

The biggest "Why Not?" is the weak-consistency guarentees of the protocol.
If our domain requires consistent membership awareness, then we should look
elsewhere:

- [Zookeeper](https://zookeeper.apache.org)
- [Paxos](http://research.microsoft.com/en-us/um/people/lamport/pubs/paxos-simple.pdf)
- [Raft](https://www.usenix.org/conference/atc14/technical-sessions/presentation/ongaro)
- [Riak Ensemble](https://github.com/basho/riak_ensemble)

What if want more than just membership awareness and fault detection? Say
we want some sort of application-level sharding like a consistent-hash ring?
SWIM and this implemention only provide the features discussed in the SWIM
paper. You can use SWIM as the underlying gossip protocol to disseminate
ring updates to the group -- but that's up to you and your application to do
everything else. You may be better off taking a look at other, more specific, implementions like:

- [Ringpop](https://github.com/uber/ringpop)
- [Plumtree](https://github.com/helium/plumtree)

What if the information we need to disseminate to the group is large, on
the order of MiB and GiB? In this implementation of SWIM we strictly use UDP for
transport and thus have an upper limit on the size of information we can
send per message. Again, we can use SWIM for membership awareness and write our
application logic using TCP to transmit our large data between members -- up to
you. It might also be worth taking a look at alternative implementations that
have modified the protocol to support both UDP and TCP:

- [Memberlist](https://github.com/hashicorp/memberlist)

### Details

If you made it this far and are still interested, you should read the module
documentation which includes details about the implementation. I also highly
recommended to read the SWIM paper referenced at the top of the documentation.
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
swim:members(lan)

```

### Build

We require OTP-18.x and an OpenSSL that supports AES-GCM. The default on OSX
does not include support for AES-GCM, so it's recommended you use `homebrew` to
install a newer version of OpenSSL and compile OTP linking to the OpenSSL managed
by `homebrew`. Include `--with-ssl=/usr/local/opt/openssl` when compiling OTP.

We use `rebar3`, included in the source of this repo, to build and test `swim`.

```

./rebar3 as test do xref, dialyzer, eunit

```