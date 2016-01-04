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

%%% @doc This module is responsible for the failure detection mechanism of the
%%% SWIM protocol.
%%%
%%% When starting the gossip process, we require the IPv4 or IPv6 address and port number
%%% to send and receive messages from other peers in the gossip group. We can
%%% also provide configuration parameters to tune the underlying protocol.
%%% The parameters we expose are:
%%%
%%% + `{protocol_period, pos_integer()}` : The time, in milliseconds, between
%%%   protocol intervals. This value is referred to as __*T*__ in the SWIM paper.
%%%   The paper notes that the protocol period has to be at least 3 times the
%%%   estimated round-trip time for a message. The protocol period is not
%%%   adjustable after the peer is started.
%%%
%%% + `{ack_timeout, pos_integer()}` : The time, in milliseconds, to wait for
%%%   an ACK in response to a PING. The ACK timeout must be less than the protocol
%%%   period. The ACK timeout should be based on the latency distribution on your
%%%   network. Good choices may be the average round-trip time or the 99th
%%%   percentile.
%%%
%%% + `{ack_proxies, pos_integer()}` : The number of peers to send indirect
%%%   probes via the PING-REQ message. This value is referred to as __*k*__
%%%   in the SWIM paper.
%%%
%%% + `{suspicion, pos_integer()}` : A scaling factor for the number of
%%%   protocol periods we wait for a suspected member to refute our claim.
%%%
%%% + `{retransmit, pos_integer()}` : A scaling factor for the number of times
%%%   we broadcast membership updates to peers in the gossip group. This value
%%%   is referred to as __*&#x3bb;*__ in the SWIM paper.
%%%
%%% + `{seeds, [member()]}` : A list of members in an existing gossip group.
%%%   When we start the local peer we attempt to join an existing group via the
%%%   provided seeds. To start a new gossip group, we can provide 0 seed members.
%%%
%%% During each protocol period defined by
%%% `protocol_period', a random member is selected from the local
%%% peer's membership list and a PING message sent to it. When a peer
%%% is first started the membership list contains the `seeds'. The
%%% local peer then waits for a reply ACK from the target of the
%%% PING. If an ACK is not received within the `ack_timeout' period,
%%% the local peer will indirectly probe the target. The local peer
%%% selects `ack_proxies' members at random, excluding the target of
%%% the original PING, and sends each a PING-REQ message. Each of
%%% these members in turn, on receiving a PING-REQ, sends a PING to
%%% the original PING target and forwards the ACK back to the local
%%% peer. If the proxies do not receive an ACK, the ACK timeout is
%%% ignored and they continue with their business. At the end of the
%%% protcol period the local peer checks if it has received any ACKs
%%% either directly from it's original target or indirectly through
%%% one of the proxies. If not, it declares the target suspicious in it's
%%% local membership list and broadcasts the update to the rest of the
%%% gossip group.
%%%
%%% The data contained in each message of the protcol is tagged with
%%% unique sequence number of the protocol period at the
%%% initiator. ACKs will contain the same sequence number contained in
%%% the original PING or PING-REQ. Upon receiving an ACK, the
%%% initiator can then distiguish between ACKs sent in response to a
%%% PING or PING-REQ during the current protocol period or ACKs sent
%%% in response to a PING from a previous protocol period where the
%%% target has already been marked as suspicious. For more information
%%% on the Supicious mechanism, see {@link swim_membership}.
%%% @end
-module(swim_gossip).
-behavior(gen_server).

-export([start_link/4,
         leave/1,
         close/1,
         members/1,
         members_with_status/1,
         gossip/2,
	 rotate_keys/2,
         stats/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).

-include("swim.hrl").

-record(state, {
	  name                            :: atom(),
	  vault                           :: pid(),
	  socket                          :: inet:socket(),
	  listen_ip                       :: inet:ip_address(),
	  me                              :: member(),
	  protocol_period                 :: pos_integer(),
	  ack_proxies                     :: pos_integer(),
	  ack_timeout                     :: pos_integer(),
	  current_ping                    :: ping(),
	  proxy_pings       = []          :: [ping()],
	  ping_targets      = []          :: [member()],
	  sequence          = 0           :: sequence(),
	  membership                      :: swim_membership:membership(),
	  broadcasts                      :: swim_broadcasts:swim_broadcast()
	 }).

-record(ping, {
	  sequence          :: sequence(),
	  ref               :: reference(),
	  tref              :: reference(),
	  terminal          :: member(),
	  origin            :: member(),
	  sent              :: erlang:monotonic_time()
	 }).

-type ping()              :: #ping{}.
-type state()             :: #state{}.
-type gossip_opt()        :: {protocol_period, pos_integer()} |
			     {ack_timeout, pos_integer()} |
			     {ack_proxies, pos_integer()} |
			     {retransmit, pos_integer()}.
-type swim_opt()          :: gossip_opt()
			   | {keys, list(key())}
			   | swim_membership:membership_opt().

-export_type([swim_opt/0]).

%% @doc Starts the Gossip process for this Node.
-spec start_link(atom(), inet:ip_address(), inet:port_number(), [gossip_opt()])
		-> {ok, pid()} | {error, term()}.
start_link(Name, ListenIp, ListenPort, Opts) ->
    gen_server:start_link({local, Name}, ?MODULE,
			  [Name, ListenIp, ListenPort, Opts], []).

%% @doc Closes the gossip peer without announcing leaving to the rest of
%%      members.
-spec close(atom()) -> ok.
close(Name) ->
    gen_server:call(Name, close).

%% @doc Closes the gossip peer after announcing leaving to the rest of
%%      members.
-spec leave(atom()) -> ok.
leave(Name) ->
    gen_server:call(Name, leave).

%% @doc Get all the members known to the gossip peer including itself. Members
%%      include both alive and suspicious members.
-spec members(atom()) -> [member()].
members(Name) ->
    gen_server:call(Name, members).

%% @doc Get all the members known to the gossip peer including itself along with
%%      the locally observed status of the member.
-spec members_with_status(atom()) -> [{member(), member_status()}].
members_with_status(Name) ->
    gen_server:call(Name, members_with_status).

-spec rotate_keys(atom(), key()) -> ok.
rotate_keys(Name, Key) ->
    gen_server:cast(Name, {rotate_keys, Key}).

%% @doc Gossip an arbitrary term to currently known peers.
%%
%% Gossiping a term to the rest of the peers piggybacks on the same protocol
%% used for disseminating membership changes. Terms provided are subject to size
%% limitations defined in {@link swim_messages:event_size_limit/0}.
-spec gossip(atom(), term()) -> ok.
gossip(Name, Event) ->
    gen_server:cast(Name, {gossip, Event}).

%% @doc Retreive protocol stats. Stats included {@link inet:getstat/1}.
-spec stats(atom()) -> list().
stats(Name) ->
    gen_server:call(Name, stats).

-spec parse_opts([swim_opt()]) -> {[swim_membership:membership_opt()], state()} | no_return().
parse_opts(Opts) ->
    parse_opts(Opts, #state{}).

-spec parse_opts([swim_opt()], state())
		-> {[swim_membership:membership_opt()], state()} | no_return().
parse_opts(Opts, State) ->
    parse_opts(Opts, [], State).

-spec parse_opts([swim_opt()], [swim_membership:membership_opt()], state())
		-> {[swim_membership:membership_opt()], state()} | no_return().
parse_opts([], MembershipOpts, State) ->
    {MembershipOpts, State};
parse_opts([{protocol_period, _} = Opt | Opts], MembershipOpts, State) ->
    parse_opts(Opts, MembershipOpts, handle_gossip_opt(Opt, State));
parse_opts([{ack_timeout, _} = Opt | Opts], MembershipOpts, State) ->
    parse_opts(Opts, MembershipOpts, handle_gossip_opt(Opt, State));
parse_opts([{ack_proxies, _} = Opt | Opts], MembershipOpts, State) ->
    parse_opts(Opts, MembershipOpts, handle_gossip_opt(Opt, State));
parse_opts([{retransmit, _} = Opt | Opts], MembershipOpts, State) ->
    parse_opts(Opts, MembershipOpts, handle_gossip_opt(Opt, State));
parse_opts([Opt | Opts], MembershipOpts, State) ->
    parse_opts(Opts, [handle_membership_opt(Opt) | MembershipOpts], State).

-spec fetch_keys(list()) -> {[key()], list()} | no_return().
fetch_keys(Opts) ->
    case lists:keytake(keys, 1, Opts) of
	{value, {keys, Keys}, NewOpts} ->
	    {Keys, NewOpts};
	false  ->
	    throw({error, {missing_keys, Opts}})
    end.

-spec handle_gossip_opt(gossip_opt(), state()) -> state() | no_return().
handle_gossip_opt({protocol_period, Val}, State) when is_integer(Val), Val > 0 ->
    State#state{protocol_period=Val};
handle_gossip_opt({ack_timeout, Val}, State) when is_integer(Val), Val > 0 ->
    State#state{ack_timeout=Val};
handle_gossip_opt({ack_proxies, Val}, State) when is_integer(Val), Val > 0 ->
    State#state{ack_proxies=Val};
handle_gossip_opt({retransmit, Val}, State) when is_integer(Val), Val > 0 ->
    State#state{broadcasts=swim_broadcasts:new(Val)};
handle_gossip_opt({listen_ip, Val}, State) when is_tuple(Val) ->
    State#state{listen_ip=Val};
handle_gossip_opt(Opt, _State) ->
    throw({error, {eoptions, Opt}}).

-spec handle_membership_opt(swim_membership:membership_opt())
			   -> swim_membership:membership_opt() | no_return().
handle_membership_opt({suspicion, Val} = Opt) when is_integer(Val), Val > 0 ->
    Opt;
handle_membership_opt({seeds, Val} = Opt) when is_list(Val) ->
    Opt;
handle_membership_opt(Opt) ->
    throw({error, {eoptions, Opt}}).

-spec create_gossip_state(inet:ip_address(), inet:port_number(), [gossip_opt()]) -> state().
create_gossip_state(AdvertiseIp, ListenPort, Opts) ->
    Me = {AdvertiseIp, ListenPort},
    apply_default_opts(create_gossip_membership(Me, parse_opts(Opts))).

-spec create_gossip_membership(member(), {[swim_membership:membership_opt()], state()}) -> state().
create_gossip_membership(Me, {MembershipOpts, State}) ->
    Membership = swim_membership:new(Me, MembershipOpts),
    State#state{membership=Membership, me=Me}.

-spec apply_default_opts(state()) -> state().
apply_default_opts(#state{protocol_period=undefined} = State) ->
    apply_default_opts(State#state{protocol_period=5000});
apply_default_opts(#state{ack_timeout=undefined} = State) ->
    apply_default_opts(State#state{ack_timeout=1000});
apply_default_opts(#state{ack_proxies=undefined} = State) ->
    apply_default_opts(State#state{ack_proxies=3});
apply_default_opts(#state{broadcasts=undefined} = State) ->
    apply_default_opts(State#state{broadcasts=swim_broadcasts:new(5)});
apply_default_opts(#state{listen_ip=undefined} = State) ->
    apply_default_opts(State#state{listen_ip={0,0,0,0}});
apply_default_opts(State) ->
    State.

-spec open_socket(inet:port_number(), list(), state()) -> state().
open_socket(ListenPort, Opts, State) ->
    #state{listen_ip=ListenIp} = State,
    SocketOpts = [{ip, ListenIp} | Opts],
    {ok, Socket} = gen_udp:open(ListenPort, SocketOpts),
    State#state{socket=Socket}.

socket_opts() ->
    [binary, {active, once}].

%% @private
init([Name, AdvertiseIp, ListenPort, Opts]) ->
    _Ran = random:seed(erlang:monotonic_time()),
    {Keys, GossipOpts} = fetch_keys(Opts),
    State = open_socket(ListenPort, socket_opts(),
			create_gossip_state(AdvertiseIp, ListenPort, GossipOpts)),
    {ok, Vault} = swim_vault:start_link(Keys),
    self() ! protocol_period,
    {ok, State#state{vault=Vault, name=Name}}.

%% @private
handle_call(stats, _From, State) ->
    #state{socket=Socket} = State,
    {ok, Stats} = inet:getstat(Socket),
    {reply, Stats, State};
handle_call(close, _From, State) ->
    {stop, normal, ok, State};
handle_call(leave, _From, State) ->
    ok = send_leave(State),
    {stop, normal, ok, State};
handle_call(members_with_status, _From, State) ->
    #state{membership=Membership} = State,
    {reply, swim_membership:members_with_status(Membership), State};
handle_call(members, _From, State) ->
    #state{membership=Membership} = State,
    {reply, swim_membership:members(Membership), State}.

%% @private
handle_cast({gossip, Event}, State) ->
    #state{broadcasts=Broadcasts} = State,
    NewBroadcasts = swim_broadcasts:push({user, Event}, Broadcasts),
    {noreply, State#state{broadcasts=NewBroadcasts}};
handle_cast({rotate_keys, Key}, State) ->
    #state{vault=Vault} = State,
    ok = swim_vault:rotate_keys(Vault, Key),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(protocol_period, State) ->
    #state{sequence=Seq} = State,
    NewState = handle_protocol_period(age_members(State#state{sequence=Seq+1})),
    {noreply, tick(NewState)};
handle_info({ack_timeout, Ref}, State) ->
    {noreply, handle_ack_timeout(Ref, State)};
handle_info({udp, Socket, Ip, InPortNo, Packet}, #state{socket=Socket} = State) ->
    NewState = handle_udp(Packet, {Ip, InPortNo}, State),
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, NewState}.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

-spec handle_udp(binary(), member(), state()) -> state().
handle_udp(Packet, From, State) ->
    #state{vault=Vault} = State,
    case swim_vault:decrypt(Vault, Packet) of
	{ok, SwimMessage} ->
	    handle_swim_message(catch(swim_messages:decode(SwimMessage)), From, State);
	{error, failed_verification} ->
	    ok = error_logger:warning_msg("Message Failed verification\n"),
	    State
    end.

-spec handle_swim_message(swim_message(), member(), state()) -> state().
handle_swim_message({ack, Seq, Responder, Events}, _From, State) ->
    process_events(Events, handle_ack(Seq, Responder, State));
handle_swim_message({ping, Seq, Events}, Origin, State) ->
    process_events(Events, handle_ping(Seq, Origin, State));
handle_swim_message({ping_req, Seq, Terminal}, Origin, State) ->
    handle_ping_req(Seq, Origin, Terminal, State);
handle_swim_message({leave, Seq}, From, State) ->
    handle_leave(Seq, From, State);
handle_swim_message({'EXIT', _}, From, State) ->
    ok = error_logger:warning_msg("Received unmatched message from ~p\n", [From]),
    State.

-spec process_events([swim_event()], state()) -> state().
process_events([], State) ->
    State;
process_events(Incoming, State) ->
    {ToPublish, NewState} = lists:foldl(fun process_event/2, {[], State}, Incoming),
    ok = publish_events(ToPublish, State),
    NewState.

-spec process_event(swim_event(), {[swim_event()], state()})
		   -> {[swim_event()], state()}.
process_event({membership, {Status, Member, Inc}}, {ToPublish, State}) ->
    #state{membership=Membership, broadcasts=Broadcasts} = State,
    {Events, NewMembership} = swim_membership:update(Member, Status, Inc, Membership),
    NewBroadcasts = push_events(Events, Broadcasts),
    {Events ++ ToPublish, State#state{broadcasts=NewBroadcasts,
				      membership=NewMembership}};
process_event({user, _} = Event, {ToPublish, State}) ->
    {[Event | ToPublish], State}.

-spec handle_protocol_period(state()) -> state().
handle_protocol_period(#state{current_ping=undefined} = State) ->
    send_next_ping(State);
handle_protocol_period(#state{current_ping=#ping{terminal=Suspect}} = State) ->
    #state{membership=Membership, broadcasts=Broadcasts} = State,
    {Events, NewMembership} = swim_membership:suspect(Suspect, Membership),
    ok = publish_events(Events, State),
    NewBroadcasts = push_events(Events, Broadcasts),
    send_next_ping(State#state{current_ping=undefined,
			       broadcasts=NewBroadcasts,
			       membership=NewMembership}).

-spec publish_events([swim_event()], state()) -> ok.
publish_events(Events, State) ->
    #state{name=Name} = State,
    _ = [swim_gossip_events:membership(Name, Event) ||
	    {membership, Event} <- Events],
    _ = [swim_gossip_events:user(Name, Event) || {user, Event} <- Events],
    ok.

-spec age_members(state()) -> state().
age_members(State) ->
    #state{membership=Membership, broadcasts=Broadcasts} = State,
    {Events, NewMembership} = swim_membership:age_members(Membership),
    ok = publish_events(Events, State),
    NewBroadcasts = push_events(Events, Broadcasts),
    State#state{broadcasts=NewBroadcasts, membership=NewMembership}.

-spec send_next_ping(state()) -> state().
send_next_ping(#state{ping_targets=[]} = State) ->
    #state{sequence=Seq, me=Me} = State,
    case ping_targets(State) of
	[] ->
	    State;
	[PingTarget|PingTargets] ->
	    {ok, Ping, NewState} = send_ping(PingTarget, Me, Seq, State),
	    NewState#state{current_ping=Ping, ping_targets=PingTargets}
    end;
send_next_ping(#state{ping_targets=[PingTarget|PingTargets]} = State) ->
    #state{me=Me, sequence=Seq} = State,
    {ok, Ping, NewState} = send_ping(PingTarget, Me, Seq, State),
    NewState#state{current_ping=Ping, ping_targets=PingTargets}.

-spec handle_ack_timeout(reference(), state()) -> state().
handle_ack_timeout(Ref, #state{current_ping=#ping{ref=Ref} = Ping} = State) ->
    #state{ack_proxies=AckProxies, sequence=Seq} = State,
    #ping{terminal=Terminal} = Ping,
    _ = [PingReq || Proxy <- proxies(AckProxies, State),
		    PingReq <- [send_ping_req(Proxy, Terminal, Seq, State)]],
    State;
handle_ack_timeout(Ref, State) ->
    #state{proxy_pings=Pings} = State,
    case lists:keytake(Ref, #ping.ref, Pings) of
	{value, #ping{terminal=Terminal}, RestPings} ->
	    member_update(suspect, Terminal, State#state{proxy_pings=RestPings});
	false ->
	    State
    end.

-spec handle_leave(sequence(), member(), state()) -> state().
handle_leave(_Seq, From, State) ->
    #state{membership=Membership, broadcasts=Broadcasts} = State,
    {Events, NewMembership} = swim_membership:leave(From, Membership),
    NewBroadcasts = push_events(Events, Broadcasts),
    State#state{membership=NewMembership, broadcasts=NewBroadcasts}.

-spec proxies(pos_integer(), state()) -> [member()].
proxies(K, State) ->
    #state{current_ping=#ping{terminal=Terminal}} = State,
    lists:sublist([T || T <- ping_targets(State), T /= Terminal], K).

-spec ping_targets(state()) -> [member()].
ping_targets(State) ->
    #state{membership=Membership, me=Me} = State,
    shuffle([M || M <- swim_membership:members(Membership), M /= Me]).

-spec handle_ping(sequence(), member(), state()) -> state().
handle_ping(Seq, From, State) ->
    #state{me=Me} = State,
    {ok, NewState} = send_ack(From, Seq, Me, State),
    member_update(alive, From, NewState).

-spec member_update(member_status(), member(), state()) -> state().
member_update(Status, Member, State) ->
    #state{membership=Membership, broadcasts=Broadcasts} = State,
    {Events, NewMembership} = swim_membership:update(Member, Status, Membership),
    ok = publish_events(Events, State),
    NewBroadcasts = push_events(Events, Broadcasts),
    State#state{broadcasts=NewBroadcasts, membership=NewMembership}.

-spec handle_ping_req(sequence(), member(), member(), state()) -> state().
handle_ping_req(Seq, Origin, Terminal, State) ->
    #state{proxy_pings=ProxyPings} = State,
    {ok, Ping, NewState} = send_ping(Terminal, Origin, Seq, State),
    NewState#state{proxy_pings=[Ping|ProxyPings]}.

-spec handle_ack(sequence(), member(), state()) -> state().
handle_ack(Seq, Terminal, #state{current_ping=#ping{sequence=Seq, terminal=Terminal}} = State) ->
    #state{current_ping=#ping{tref=TRef}} = State,
    _ = erlang:cancel_timer(TRef),
    member_update(alive, Terminal, State#state{current_ping=undefined});
handle_ack(Seq, Terminal, State) ->
    #state{proxy_pings=Pings} = State,
    case lists:keytake(Terminal, #ping.terminal, Pings) of
	{value, #ping{origin=Origin, tref=TRef}, NewPings} ->
	    _ = erlang:cancel_timer(TRef),
	    {ok, NewState} = send_ack(Origin, Seq, Terminal, State),
	    member_update(alive, Terminal, NewState#state{proxy_pings=NewPings});
	false ->
	    State
    end.

-spec tick(state()) -> state().
tick(State) ->
    #state{protocol_period=Timeout} = State,
    _TRef = erlang:send_after(Timeout, self(), protocol_period),
    State.

-spec shuffle([member()]) -> [member()].
shuffle(List) ->
    [X || {_, X} <- lists:keysort(1, [{random:uniform(), N} || N <- List])].

-spec send_ping_req(member(), member(), sequence(), state()) -> ok.
send_ping_req({Ip, Port}, Terminal, Seq, State) ->
    #state{socket=Socket, vault=Vault} = State,
    Data = swim_vault:encrypt(Vault, swim_messages:encode_ping_req(Seq, Terminal)),
    gen_udp:send(Socket, Ip, Port, Data).

-spec send_ping(member(), member(), sequence(), state())
	       -> {ok, ping(), state()}.
send_ping({Ip, Port} = To, From, Seq, State) ->
    #state{socket=Socket, vault=Vault} = State,
    {Events, NewState} = issue_events(State),
    Data = swim_vault:encrypt(Vault, swim_messages:encode_ping(Seq, Events)),
    ok = gen_udp:send(Socket, Ip, Port, Data),
    Ref = make_ref(),
    TRef = create_ack_timer(Ref, State),
    Ping = #ping{sequence=Seq, origin=From, terminal=To, ref=Ref,
		 tref=TRef, sent=erlang:monotonic_time()},
    {ok, Ping, NewState}.

-spec send_ack(member(), sequence(), member(), state()) -> {ok, state()}.
send_ack({Ip, Port}, Seq, From, State) ->
    #state{socket=Socket, vault=Vault} = State,
    {Events, NewState} = issue_events(State),
    Data = swim_vault:encrypt(Vault, swim_messages:encode_ack(Seq, From, Events)),
    ok = gen_udp:send(Socket, Ip, Port, Data),
    {ok, NewState}.

-spec send_leave(state()) -> ok.
send_leave(State) ->
    #state{socket=Socket, ack_proxies=AckProxies, sequence=Seq} = State,
    Data = swim_messages:encode_leave(Seq),
    _ = [Leave || {Ip, Port} <- proxies(AckProxies, State),
		  Leave <- [gen_udp:send(Socket, Ip, Port, Data)]],
    ok.

-spec create_ack_timer(reference(), state()) -> reference().
create_ack_timer(Ref, State) ->
    #state{ack_timeout=Timeout} = State,
    erlang:send_after(Timeout, self(), {ack_timeout, Ref}).

-spec push_events([swim_event()], swim_broadcasts:swim_broadcast())
		 -> swim_broadcasts:swim_broadcast().
push_events([], Broadcast) ->
    Broadcast;
push_events([Event | Events], Broadcast) ->
    push_events(Events, swim_broadcasts:push(Event, Broadcast)).

-spec issue_events(state()) -> {[swim_event()], state()}.
issue_events(State) ->
    #state{broadcasts=Broadcasts, membership=Membership} = State,
    MemberSize = swim_membership:size(Membership),
    Encode = fun swim_messages:encode_event/1,
    EventSizeLimit = swim_messages:event_size_limit(),
    {Events, NewBroadcasts} = swim_broadcasts:pop(EventSizeLimit, MemberSize,
						  Encode, Broadcasts),
    {Events, State#state{broadcasts=NewBroadcasts}}.
