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

-module(swim_failure).
-behavior(gen_server).

-export([start_link/3]).
-export([probe/3]).
-export([probe/5]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-record(state, {
          local_member       :: swim:member(),
          probe              :: undefined | probe(),
          ping_reqs    = #{} :: #{{swim:member(), sequence()} := ping_req()},
          nack_timeout       :: non_neg_integer(),
          ack_timeout        :: non_neg_integer(),
          socket             :: undefined | inet:socket(),
          keyring            :: swim_keyring:keyring(),
          sequence     = 0   :: sequence()
         }).

-record(probe, {
          target      :: swim:member(),
          sequence    :: sequence(),
          ack_timer   :: reference(),
          probe_timer :: reference(),
          ack_fun     :: fun((swim:member()) -> ok),
          nack_fun    :: fun((swim:member()) -> ok)
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

-export_type([sequence/0]).

-spec start_link(PortNumber, KeyRing, NackTimeout) -> {ok, pid()} when
      PortNumber  :: inet:port_number(),
      KeyRing     :: swim_keyring:keyring(),
      NackTimeout :: pos_integer().

start_link(ListenPort, Keyring, NackTimeout) ->
    Args = [ListenPort, Keyring, NackTimeout],
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

-spec probe(Target, AckTimeout, ProbeTimeout) -> ok when
      Target       :: swim:member(),
      AckTimeout   :: pos_integer(),
      ProbeTimeout :: pos_integer().

probe(Target, AckTimeout, ProbeTimeout) ->
    probe(Target, AckTimeout, ProbeTimeout, fun swim_state:ack/1, fun swim_state:nack/1).

-spec probe(Target, AckTimeout, ProbeTimeout, AckFun, NackFun) -> ok when
      Target       :: swim:member(),
      AckTimeout   :: pos_integer(),
      ProbeTimeout :: pos_integer(),
      AckFun       :: fun((swim:member()) -> ok),
      NackFun      :: fun((swim:member()) -> ok).

probe(Target, AckTimeout, ProbeTimeout, AckFun, NackFun)
  when ProbeTimeout >= AckTimeout * 3 ->
    Msg = {probe, Target, AckTimeout, ProbeTimeout, AckFun, NackFun},
    gen_server:cast(?MODULE, Msg).

%% @private
init([ListenPort, Keyring, NackTimeout]) ->
    LocalMember = swim_state:local_member(),
    SocketOpts = [binary, {active, 16}],
    {ok, Socket} = gen_udp:open(ListenPort, SocketOpts),
    State = #state{
               local_member = LocalMember,
               keyring      = Keyring,
               socket       = Socket,
               nack_timeout = NackTimeout
              },
    {ok, State}.

%% @private
handle_call(_Msg, _From, State) ->
    {noreply, State}.

%% @private
handle_cast({probe, Target, AckTimeout, ProbeTimeout, AckFun, NackFun}, State)
  when State#state.probe =:= undefined ->
    {noreply, send_probe(Target, AckTimeout, ProbeTimeout, AckFun, NackFun, State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({udp_passive, Socket}, #state{socket = Socket} = State) ->
    ok = inet:setopts(Socket, [{active, 16}]),
    {noreply, State};
handle_info({udp, Socket, Ip, InPortNo, Packet}, #state{socket = Socket} = State) ->
    {noreply, handle_packet(Packet, {Ip, InPortNo}, State)};
handle_info({probe_timeout, Target, Sequence}, #state{probe = Probe} = State)
  when Probe#probe.target =:= Target andalso Probe#probe.sequence =:= Sequence ->
    {noreply, State#state{probe = undefined}};
handle_info({ack_timeout, Target, Sequence}, #state{probe = Probe} = State)
  when Probe#probe.target =:= Target andalso Probe#probe.sequence =:= Sequence ->
    _ = swim_time:cancel_timer(Probe#probe.ack_timer),
    Msg = {ping_req, Sequence, Probe#probe.target},
    [send(Proxy, Msg, State) || Proxy <- swim_state:proxies(Target)],
    {noreply, State};
handle_info({ack_timeout, Target, Sequence}, State) ->
    case maps:take({Target, Sequence}, State#state.ping_reqs) of
        {_, PingReqs} ->
            {noreply, State#state{ping_reqs = PingReqs}};
        error ->
            State
    end;
handle_info({nack_timeout, Target, Sequence}, State) ->
    case maps:find({Target, Sequence}, State#state.ping_reqs) of
        {ok, #ping_req{origin = Origin, sequence = OriginSequence}} ->
            Msg = {nack, OriginSequence, Origin},
            ok = send(Origin, Msg, State),
            {noreply, State};
        error ->
            State
    end;
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
terminate(_Reason, #state{socket = undefined}) ->
    ok;
terminate(_Reason, #state{socket = Socket}) ->
    gen_udp:close(Socket).

send_probe(Target, AckTimeout, ProbeTimeout, AckFun, NackFun, State) ->
    NextSequence = State#state.sequence + 1,
    Msg = {ping, NextSequence, Target},
    case send(Target, Msg, State) of
        ok ->
            AckTimer = start_ack_timer(AckTimeout, Target, NextSequence),
            ProbeTimer = start_probe_timer(ProbeTimeout, Target, NextSequence),
            Probe = #probe{
                       target      = Target,
                       sequence    = NextSequence,
                       ack_timer   = AckTimer,
                       probe_timer = ProbeTimer,
                       ack_fun     = AckFun,
                       nack_fun    = NackFun
                      },
            State#state{probe = Probe, sequence = NextSequence};
        {error, _Reason} ->
            State
    end.

handle_packet(Packet, Peer, State) ->
    case decrypt(Packet, State) of
        {ok, PlainText} ->
            try
                {Message, Events} = swim_messages:decode(PlainText),
                spawn_link(fun() -> handle_events(Events) end),
                handle_message(Message, Peer, State)
            catch
                _:_ ->
                    State
            end;
        {error, failed_verification} ->
            State
    end.

handle_message({ack, Sequence, Terminal}, _Peer, State) ->
    handle_ack(Sequence, Terminal, State);
handle_message({nack, Sequence, Terminal}, _Peer, State) ->
    handle_nack(Sequence, Terminal, State);
handle_message({ping, Sequence, Target}, Peer, State) ->
    handle_ping(Target, Sequence, Peer, State);
handle_message({ping_req, Sequence, Terminal}, Peer, State) ->
    handle_ping_req(Sequence, Terminal, Peer, State).

handle_ack(Sequence, Responder, #state{probe = Probe} = State)
  when Responder =:= Probe#probe.target andalso Probe#probe.sequence =:= Sequence ->
    #probe{ack_fun = AckFun, ack_timer = AckTimer, probe_timer = ProbeTimer} = Probe,
    ok = AckFun(Responder),
    swim_time:cancel_timer(AckTimer),
    swim_time:cancel_timer(ProbeTimer),
    State#state{probe = undefined};
handle_ack(Sequence, Responder, State) ->
    case maps:take({Responder, Sequence}, State#state.ping_reqs) of
        {PingReq, PingReqs} ->
            Msg = {ack, PingReq#ping_req.sequence, Responder},
            ok = send(PingReq#ping_req.origin, Msg, State),
            swim_time:cancel_timer(PingReq#ping_req.ack_timer),
            swim_time:cancel_timer(PingReq#ping_req.nack_timer),
            State#state{ping_reqs = PingReqs};
        error ->
            State
    end.

handle_nack(Sequence, Responder, #state{probe = Probe} = State)
  when Responder =:= Probe#probe.target andalso Probe#probe.sequence =:= Sequence ->
    #probe{nack_fun = NackFun} = Probe,
    ok = NackFun(Responder),
    State;
handle_nack(_Sequence, _Responder, State) ->
    State.

handle_ping(Target, Sequence, Peer, #state{local_member = Target} = State) ->
    Msg = {ack, Sequence, Target},
    _ = send(Peer, Msg, State),
    State;
handle_ping(_Target, _Sequence, _Peer, State) ->
    State.

handle_ping_req(OriginSequence, Terminal, Origin, State) ->
    NextSequence = State#state.sequence + 1,
    Msg = {ping, NextSequence, Terminal},
    case send(Terminal, Msg, State) of
        ok ->
            NackTimer = start_nack_timer(State#state.nack_timeout, Terminal, NextSequence),
            AckTimer = start_ack_timer(State#state.ack_timeout, Terminal, NextSequence),
            PingReqs = maps:put({Terminal, NextSequence},
                                {Origin, OriginSequence, NackTimer, AckTimer},
                                State#state.ping_reqs),
            State#state{ping_reqs = PingReqs, sequence = NextSequence};
        {error, _Reason} ->
            State
    end.

send({DestIp, DestPort}, Msg, State) ->
    Events = swim_state:broadcasts(3),
    Payload = encrypt(swim_messages:encode({Msg, Events}), State),
    gen_udp:send(State#state.socket, DestIp, DestPort, Payload).

start_ack_timer(Timeout, Terminal, Sequence) ->
    swim_time:send_after(Timeout, self(), {ack_timeout, Terminal, Sequence}).

start_nack_timer(Timeout, Terminal, Sequence) ->
    swim_time:send_after(Timeout, self(), {nack_timeout, Terminal, Sequence}).

start_probe_timer(Timeout, Target, Sequence) ->
    swim_time:send_after(Timeout, self(), {probe_timeout, Target, Sequence}).

encrypt(Msg, State) ->
    swim_keyring:encrypt(Msg, State#state.keyring).

decrypt(CipherText, State) ->
    swim_keyring:decrypt(CipherText, State#state.keyring).

handle_events(Events) ->
    [swim_state:handle_event(Event) || {Category, _} = Event <- Events,
                                       Category =:= membership],
    ok.
