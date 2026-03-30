%%% ----------------------------------------------------------------------------
%%% Copyright (c) 2015-2017 All Rights Reserved.
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

%%% @copyright 2015-2017
%%% @version {@version}

-module(swim_state).
-behavior(gen_server).

-export([start_link/6]).
-export([stop/0]).
-export([local_member/0]).
-export([local_state/0]).
-export([members/0]).
-export([handle_event/1]).
-export([publish/1]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-record(state, {
          %% Protocol parameters
          protocol_period        :: pos_integer(),
          ack_timeout            :: pos_integer(),
          probe_timeout          :: pos_integer(),
          nack_timeout           :: non_neg_integer(),
          num_proxies            :: pos_integer(),

          %% Membership state
          current_probe          :: undefined | {swim:member(), swim:incarnation()},
          membership             :: swim_membership:membership(),
          broadcasts             :: swim_broadcasts:broadcasts(),
          awareness              :: swim_awareness:awareness(),

          %% Network / failure detection state
          local_member           :: swim:member(),
          socket                 :: undefined | inet:socket(),
          keyring                :: swim_keyring:keyring(),
          probe                  :: undefined | probe(),
          ping_reqs        = #{} :: #{{swim:member(), sequence()} := ping_req()},
          sequence         = 0   :: sequence()
         }).

-record(probe, {
          target            :: swim:member(),
          sequence          :: sequence(),
          ack_timer         :: reference(),
          probe_timer       :: reference(),
          missing_nacks = 0 :: non_neg_integer()
         }).

-record(ping_req, {
          origin     :: swim:member(),
          sequence   :: sequence(),
          ack_timer  :: reference(),
          nack_timer :: reference()
         }).

-type ping_req() :: #ping_req{}.
-type probe()    :: #probe{}.
-type sequence() :: non_neg_integer().

start_link(LocalMember, Keyring, Membership, Broadcasts, Awareness, Opts) ->
    Args = [LocalMember, Keyring, Membership, Broadcasts, Awareness, Opts],
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

-spec stop() -> ok.

stop() ->
    gen_server:stop(?MODULE).

local_member() ->
    gen_server:call(?MODULE, local_member).

local_state() ->
    gen_server:call(?MODULE, local_state).

members() ->
    gen_server:call(?MODULE, members).

publish(Event) ->
    gen_server:cast(?MODULE, {publish, Event}).

handle_event(Event) ->
    gen_server:cast(?MODULE, {broadcast_event, Event}).

%% @private
init([{_, Port} = LocalMember, Keyring, Membership, Broadcasts, Awareness, Opts]) ->
    SocketOpts = [binary, {active, 16}],
    {ok, Socket} = swim_socket:open(Port, SocketOpts),
    State =
        #state{
           local_member    = LocalMember,
           keyring         = Keyring,
           socket          = Socket,
           membership      = Membership,
           broadcasts      = Broadcasts,
           awareness       = Awareness,
           ack_timeout     = maps:get(ack_timeout, Opts),
           probe_timeout   = maps:get(probe_timeout, Opts),
           nack_timeout    = maps:get(nack_timeout, Opts),
           protocol_period = maps:get(protocol_period, Opts),
           num_proxies     = maps:get(num_proxies, Opts)
          },
    self() ! protocol_period,
    {ok, State}.

%% @private
handle_call(local_member, _From, State) ->
    {reply, swim_membership:local_member(State#state.membership), State};
handle_call(members, _From, State) ->
    {reply, swim_membership:members(State#state.membership), State};
handle_call(local_state, _From, State) ->
    {reply, swim_membership:local_state(State#state.membership), State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

%% @private
handle_cast({publish, Event}, State) ->
    Broadcasts = swim_broadcasts:insert({user, Event}, State#state.broadcasts),
    {noreply, State#state{broadcasts = Broadcasts}};
handle_cast({broadcast_event, Event}, State) ->
    {noreply, apply_membership_event(Event, State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(protocol_period, State) ->
    NewState = handle_protocol_period(State),
    schedule_next_protocol_period(NewState),
    {noreply, NewState};
handle_info({suspicion_timeout, Member, SuspectedAt}, State) ->
    {Events, TimerActions, Membership0} =
        swim_membership:faulty(Member, SuspectedAt, local, State#state.membership),
    Membership = handle_timer_actions(TimerActions, Membership0),
    Broadcasts = swim_broadcasts:insert(Events, State#state.broadcasts),
    ok = swim_subscriptions:publish(Events),
    {noreply, State#state{membership = Membership, broadcasts = Broadcasts}};
handle_info({udp_passive, Socket}, #state{socket = Socket} = State) ->
    ok = swim_socket:setopts(Socket, [{active, 16}]),
    {noreply, State};
handle_info({udp, Socket, Ip, InPortNo, Packet}, #state{socket = Socket} = State) ->
    {noreply, handle_packet(Packet, {Ip, InPortNo}, State)};
handle_info({probe_timeout, Target, Sequence}, State) ->
    {noreply, handle_probe_timeout(Target, Sequence, State)};
handle_info({ack_timeout, Target, Sequence}, State) ->
    {noreply, handle_ack_timeout(Target, Sequence, State)};
handle_info({nack_timeout, Target, Sequence}, State) ->
    {noreply, handle_nack_timeout(Target, Sequence, State)};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
terminate(_Reason, #state{socket = undefined}) ->
    ok;
terminate(_Reason, #state{socket = Socket}) ->
    swim_socket:close(Socket).

%%% ===================================================================
%%% Protocol period
%%% ===================================================================

handle_protocol_period(State) ->
    case swim_membership:probe_target(State#state.membership) of
        none ->
            State;
        {{Target, _} = CurrentProbe, Membership} ->
            ProbeTimeout = swim_awareness:scale(State#state.probe_timeout, State#state.awareness),
            AckTimeout = State#state.ack_timeout,
            NewState = send_probe(Target, AckTimeout, ProbeTimeout,
                                  State#state{membership = Membership}),
            NewState#state{current_probe = CurrentProbe}
    end.

schedule_next_protocol_period(State) ->
    #state{awareness = Awareness, protocol_period = ProtocolPeriod} = State,
    Timeout = swim_awareness:scale(ProtocolPeriod, Awareness),
    swim_time:send_after(Timeout, self(), protocol_period).

%%% ===================================================================
%%% Failure detection: probing
%%% ===================================================================

send_probe(Target, AckTimeout, ProbeTimeout, State)
  when ProbeTimeout >= AckTimeout * 3 ->
    NextSequence = State#state.sequence + 1,
    Msg = {ping, NextSequence, Target},
    NewState = send(Target, Msg, State),
    AckTimer = start_ack_timer(AckTimeout, Target, NextSequence),
    ProbeTimer = start_probe_timer(ProbeTimeout, Target, NextSequence),
    Probe = #probe{
               target      = Target,
               sequence    = NextSequence,
               ack_timer   = AckTimer,
               probe_timer = ProbeTimer
              },
    swim_metrics:notify({probe, Target}),
    NewState#state{probe = Probe, sequence = NextSequence}.

%%% ===================================================================
%%% Failure detection: packet handling
%%% ===================================================================

handle_packet(Packet, Peer, State) ->
    case decrypt(Packet, State) of
        {ok, PlainText} ->
            try
                {Message, Events} = swim_messages:decode(PlainText),
                swim_metrics:notify({rx, iolist_size(Packet)}),
                State1 = handle_events(Events, State),
                handle_message(Message, Peer, State1)
            catch
                _:_ ->
                    State
            end;
        {error, failed_verification} ->
            State
    end.

handle_message({ack, Sequence, Terminal}, _Peer, State) ->
    swim_metrics:notify({ack, Terminal}),
    handle_ack(Sequence, Terminal, State);
handle_message({nack, Sequence, Terminal}, Peer, State) ->
    swim_metrics:notify({nack, Terminal, Peer}),
    handle_nack(Sequence, Terminal, State);
handle_message({ping, Sequence, Target}, Peer, State) ->
    swim_metrics:notify({ping, Peer}),
    handle_ping(Target, Sequence, Peer, State);
handle_message({ping_req, Sequence, Terminal}, Peer, State) ->
    swim_metrics:notify({ping_req, Terminal, Peer}),
    handle_ping_req(Sequence, Terminal, Peer, State).

handle_ack(Sequence, Responder, #state{probe = Probe} = State)
  when Responder =:= Probe#probe.target andalso Probe#probe.sequence =:= Sequence ->
    #probe{ack_timer = AckTimer, probe_timer = ProbeTimer} = Probe,
    swim_time:cancel_timer(AckTimer, [{async, true}, {info, false}]),
    swim_time:cancel_timer(ProbeTimer, [{async, true}, {info, false}]),
    #state{current_probe = {Responder, Incarnation}} = State,
    handle_member_ack(Responder, Incarnation, State#state{probe = undefined});
handle_ack(Sequence, Responder, State) ->
    case maps:take({Responder, Sequence}, State#state.ping_reqs) of
        {PingReq, PingReqs} ->
            Msg = {ack, PingReq#ping_req.sequence, Responder},
            NewState = send(PingReq#ping_req.origin, Msg, State),
            swim_time:cancel_timer(PingReq#ping_req.ack_timer, [{async, true}, {info, false}]),
            swim_time:cancel_timer(PingReq#ping_req.nack_timer, [{async, true}, {info, false}]),
            NewState#state{ping_reqs = PingReqs};
        error ->
            State
    end.

handle_nack(Sequence, Target, #state{probe = Probe} = State)
  when Target =:= Probe#probe.target andalso Probe#probe.sequence =:= Sequence ->
    #probe{missing_nacks = MissingNacks} = Probe,
    State#state{probe = Probe#probe{missing_nacks = MissingNacks - 1}};
handle_nack(_Sequence, _Target, State) ->
    State.

handle_ping(Target, Sequence, Peer, #state{local_member = Target} = State) ->
    Msg = {ack, Sequence, Target},
    send(Peer, Msg, State);
handle_ping(_Target, _Sequence, _Peer, State) ->
    State.

handle_ping_req(OriginSequence, Terminal, Origin, State) ->
    NextSequence = State#state.sequence + 1,
    Msg = {ping, NextSequence, Terminal},
    NewState = send(Terminal, Msg, State),
    NackTimer = start_nack_timer(State#state.nack_timeout, Terminal, NextSequence),
    AckTimer = start_ack_timer(State#state.ack_timeout, Terminal, NextSequence),
    PingReq = #ping_req{origin = Origin, sequence = OriginSequence,
                        ack_timer = AckTimer, nack_timer = NackTimer},
    PingReqs = maps:put({Terminal, NextSequence}, PingReq, State#state.ping_reqs),
    NewState#state{ping_reqs = PingReqs, sequence = NextSequence}.

%%% ===================================================================
%%% Failure detection: timeouts
%%% ===================================================================

handle_ack_timeout(Target, Sequence, #state{probe = Probe} = State)
  when Probe#probe.target =:= Target andalso Probe#probe.sequence =:= Sequence ->
    swim_metrics:notify({ack_timeout, Target}),
    swim_time:cancel_timer(Probe#probe.ack_timer, [{async, true}, {info, false}]),
    Msg = {ping_req, Sequence, Probe#probe.target},
    Proxies = swim_membership:proxies(State#state.num_proxies, Target, State#state.membership),
    NewState = lists:foldl(fun(Proxy, S) -> send(Proxy, Msg, S) end, State, Proxies),
    NewState#state{probe = Probe#probe{missing_nacks = length(Proxies)}};
handle_ack_timeout(Target, Sequence, State) ->
    case maps:take({Target, Sequence}, State#state.ping_reqs) of
        {_, PingReqs} ->
            swim_metrics:notify({ack_timeout, Target}),
            State#state{ping_reqs = PingReqs};
        error ->
            State
    end.

handle_nack_timeout(Target, Sequence, State) ->
    case maps:find({Target, Sequence}, State#state.ping_reqs) of
        {ok, #ping_req{origin = Origin, sequence = OriginSequence}} ->
            swim_metrics:notify({nack_timeout, Target, Origin}),
            Msg = {nack, OriginSequence, Origin},
            send(Origin, Msg, State);
        error ->
            State
    end.

handle_probe_timeout(Target, Sequence, #state{probe = Probe} = State)
  when Probe#probe.target =:= Target andalso Probe#probe.sequence =:= Sequence ->
    swim_metrics:notify({probe_timeout, Target}),
    #state{current_probe = {Target, Incarnation}} = State,
    handle_member_probe_timeout(Target, Incarnation, Probe#probe.missing_nacks,
                                State#state{probe = undefined});
handle_probe_timeout(_Target, _Sequence, State) ->
    State.

%%% ===================================================================
%%% Membership state transitions
%%% ===================================================================

handle_member_ack(Member, Incarnation, State) ->
    #state{membership = Membership0, broadcasts = Broadcasts0, awareness = Awareness0} = State,
    {Events, TimerActions, Membership1} = swim_membership:alive(Member, Incarnation, Membership0),
    Membership = handle_timer_actions(TimerActions, Membership1),
    Broadcasts = swim_broadcasts:insert(Events, Broadcasts0),
    ok = swim_subscriptions:publish(Events),
    Awareness = swim_awareness:success(Awareness0),
    State#state{
      membership    = Membership,
      broadcasts    = Broadcasts,
      awareness     = Awareness,
      current_probe = undefined
     }.

handle_member_probe_timeout(Member, Incarnation, MissedNacks, State) ->
    #state{membership = Membership0, broadcasts = Broadcasts0, awareness = Awareness0} = State,
    {Events, TimerActions, Membership1} = swim_membership:suspect(Member, Incarnation, local, Membership0),
    Membership = handle_timer_actions(TimerActions, Membership1),
    Broadcasts = swim_broadcasts:insert(Events, Broadcasts0),
    ok = swim_subscriptions:publish(Events),
    Awareness = swim_awareness:failure(MissedNacks + 1, Awareness0),
    State#state{
      current_probe = undefined,
      membership    = Membership,
      broadcasts    = Broadcasts,
      awareness     = Awareness
     }.

apply_membership_event(Event, State) ->
    {Events, TimerActions, Membership0} = swim_membership:handle_event(Event, State#state.membership),
    Membership = handle_timer_actions(TimerActions, Membership0),
    Awareness =
        case swim_membership:refuted(Events, Membership) of
            true -> swim_awareness:failure(State#state.awareness);
            false -> State#state.awareness
        end,
    Broadcasts = swim_broadcasts:insert(Events, State#state.broadcasts),
    ok = swim_subscriptions:publish(Events),
    State#state{membership = Membership, broadcasts = Broadcasts, awareness = Awareness}.

%%% ===================================================================
%%% Event handling (from piggybacked messages)
%%% ===================================================================

handle_events(Events, State) ->
    {MembershipEvents, UserEvents} =
        lists:partition(fun({membership, _}) -> true; (_) -> false end, Events),
    State1 = lists:foldl(fun(Event, S) -> apply_membership_event(Event, S) end,
                         State, MembershipEvents),
    [swim_subscriptions:publish(Event) || Event <- UserEvents],
    State1.

%%% ===================================================================
%%% Network: send and encryption
%%% ===================================================================

send({DestIp, DestPort} = Target, Msg, State) ->
    #state{membership = Membership, broadcasts = Broadcasts0} = State,
    {Events, Broadcasts1} = swim_broadcasts:take(Target, Broadcasts0),
    NumMembers = swim_membership:size(Membership),
    Retransmits = swim_broadcasts:retransmit_limit(NumMembers, Broadcasts1),
    Broadcasts2 = swim_broadcasts:prune(Retransmits, Broadcasts1),
    Payload = encrypt(swim_messages:encode({Msg, Events}), State),
    ok = swim_socket:send(State#state.socket, DestIp, DestPort, Payload),
    swim_metrics:notify({tx, iolist_size(Payload)}),
    State#state{broadcasts = Broadcasts2}.

encrypt(Msg, State) ->
    swim_keyring:encrypt(Msg, State#state.keyring).

decrypt(CipherText, State) ->
    swim_keyring:decrypt(CipherText, State#state.keyring).

%%% ===================================================================
%%% Timers
%%% ===================================================================

start_ack_timer(Timeout, Terminal, Sequence) ->
    swim_time:send_after(Timeout, self(), {ack_timeout, Terminal, Sequence}).

start_nack_timer(Timeout, Terminal, Sequence) ->
    swim_time:send_after(Timeout, self(), {nack_timeout, Terminal, Sequence}).

start_probe_timer(Timeout, Target, Sequence) ->
    swim_time:send_after(Timeout, self(), {probe_timeout, Target, Sequence}).

handle_timer_actions([], Membership) ->
    Membership;
handle_timer_actions([{start_suspicion_timer, Timeout, Member, Inc} | Rest], Membership) ->
    TRef = swim_time:send_after(Timeout, self(), {suspicion_timeout, Member, Inc}),
    Now = swim_time:monotonic_time(),
    handle_timer_actions(Rest, swim_membership:set_suspicion_timer(Member, TRef, Now, Membership));
handle_timer_actions([{cancel_suspicion_timer, TRef} | Rest], Membership) ->
    swim_time:cancel_timer(TRef, [{async, true}, {info, false}]),
    handle_timer_actions(Rest, Membership).
