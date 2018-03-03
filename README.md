# SWIM - An Awesome Weakly-consistent Infection-style Gossip Protocol #

Copyright (c) 2015-2018 Tucker Barbour

__Authors:__ Tucker Barbour ([`tucker.barbour@gmail.com`](mailto:tucker.barbour@gmail.com)).

__References__* http://www.cs.cornell.edu/~asdas/research/dsn02-SWIM.pdf

(__WARNING:__ This project is untested in production environments. Do not use in production.)

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

### Project Status

This project is still under active development and as such the API may change without warning.

### Use Cases

What can we use SWIM for?

- Reliable multicast
- Epidemic-style information dissemination
- Pub-sub
- Generic peer-to-peer systems

Really anything that requires each process in a group to maintain a local list
of other non-faulty processes in the group and be notified when members join or
leave, either voluntarily or through failure.

### Why?

Other distributed membership algorithms tradionally use a heartbeating technique.
The heartbeat technique calls for each process in the group to periodically
send an incrementing heartbeat counter to all other processes in the group as well
as respond to incoming heartbeats from other process. A process is detected as
faulty when a heartbeat response is not received from a process in some
period of time. Heartbeat implementations often suffer from scalability limitiations
as the size of the process group grows. Some popular heartbeat architectures
along with potential weaknesses:

* Centralized - leads to hot-spots and single-point-of-failure
* All-to-All - leads to message load on the network that grows quadratically with the group size
* Logical Ring - unpredicability of failure detection time

SWIM addresses the problems with tradional heartbeat implementations through a
peer-to-peer randomized probing protocol. I recommend reading the SWIM paper
for more details.

### Why Not?

SWIM provides weak-consistency guarentees of group membership.
If our domain requires stronger consistency of membership awareness then we
should look elsewhere:

- [Zookeeper](https://zookeeper.apache.org)
- [Paxos](http://research.microsoft.com/en-us/um/people/lamport/pubs/paxos-simple.pdf)
- [Raft](https://www.usenix.org/conference/atc14/technical-sessions/presentation/ongaro)
- [Riak Ensemble](https://github.com/basho/riak_ensemble)

What if we want more than just membership awareness and fault detection? Say
we want application-level sharding like a consistent-hash ring?
SWIM and this implemention only provide weakly-consistent membership awareness.
You can use SWIM as the underlying gossip protocol to disseminate
ring updates to the group -- but that's up to you and your application. You may
be better off taking a look at other, more specific, implementions like:

- [Ringpop](https://github.com/uber/ringpop)
- [Plumtree](https://github.com/helium/plumtree)

What if the information we need to disseminate to the group is large, on
the order of MiB and GiB? This implementation of SWIM uses UDP for
transport and thus has an upper limit on the size of information we can
reliably send per message. Again, we can use SWIM for membership awareness and
write our application logic using TCP to transmit our large data between members.
It might also be worth taking a look at alternative implementations that have
modified the protocol to support both UDP and TCP:

- [Memberlist](https://github.com/hashicorp/memberlist)

### Lifeguard

We've also included some of the improvements outlined in the [Lifeguard](https://arxiv.org/abs/1707.00788) paper from Hashicorp. You can also find more information about their research on [their website](https://www.hashicorp.com/blog/making-gossip-more-robust-with-lifeguard). On a local 5 node cluster, we have observed a reduction in false positives rates during the threshold experiment. More details of the results will be provided when we have time to conduct a more scientific experiement with this implementation.

### Build

We require OTP-19.x and an OpenSSL that supports AES-GCM. The default on OSX
does not include support for AES-GCM, so it's recommended you use `homebrew` to
install a newer version of OpenSSL and compile OTP linking to the OpenSSL managed
by `homebrew`. Include `--with-ssl=/usr/local/opt/openssl` when compiling OTP.


