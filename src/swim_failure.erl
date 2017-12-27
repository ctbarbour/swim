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

-export([start_link/4]).
-export([stop/0]).
-export([probe/3]).
-export([probe/4]).

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
          target            :: swim:member(),
          sequence          :: sequence(),
          ack_timer         :: reference(),
          probe_timer       :: reference(),
          ack_fun           :: fun((swim:member()) -> ok),
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

-export_type([sequence/0]).

-spec start_link(Member, KeyRing, AckTimeout, NackTimeout) -> {ok, pid()} when
      Member      :: swim:member(),
      KeyRing     :: swim_keyring:keyring(),
      AckTimeout  :: pos_integer(),
      NackTimeout :: pos_integer().

start_link(LocalMember, Keyring, AckTimeout, NackTimeout) ->
    Args = [LocalMember, Keyring, AckTimeout, NackTimeout],
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

-spec stop() -> ok.

stop() ->
    gen_server:stop(?MODULE).

-spec probe(Target, AckTimeout, ProbeTimeout) -> ok when
      Target       :: swim:member(),
      AckTimeout   :: pos_integer(),
      ProbeTimeout :: pos_integer().

probe(Target, AckTimeout, ProbeTimeout) ->
    probe(Target, AckTimeout, ProbeTimeout, fun swim_state:ack/1).

-spec probe(Target, AckTimeout, ProbeTimeout, AckFun) -> ok when
      Target       :: swim:member(),
      AckTimeout   :: pos_integer(),
      ProbeTimeout :: pos_integer(),
      AckFun       :: fun((swim:member()) -> ok).

probe(Target, AckTimeout, ProbeTimeout, AckFun)
  when ProbeTimeout >= AckTimeout * 3 ->
    Msg = {probe, Target, AckTimeout, ProbeTimeout, AckFun},
    gen_server:cast(?MODULE, Msg).

%% @private
init([{_, Port} = LocalMember, Keyring, AckTimeout, NackTimeout]) ->
    SocketOpts = [binary, {active, 16}],
    {ok, Socket} = swim_socket:open(Port, SocketOpts),
    State = #state{
               local_member = LocalMember,
               keyring      = Keyring,
               socket       = Socket,
               ack_timeout  = AckTimeout,
               nack_timeout = NackTimeout
              },
    {ok, State}.

%% @private
handle_call(_Msg, _From, State) ->
    {noreply, State}.

%% @private
handle_cast({probe, Target, AckTimeout, ProbeTimeout, AckFun}, State)
  when State#state.probe =:= undefined ->
    {noreply, send_probe(Target, AckTimeout, ProbeTimeout, AckFun, State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
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

send_probe(Target, AckTimeout, ProbeTimeout, AckFun, State) ->
    NextSequence = State#state.sequence + 1,
    Msg = {ping, NextSequence, Target},
    NewState = send(Target, Msg, State),
    AckTimer = start_ack_timer(AckTimeout, Target, NextSequence),
    ProbeTimer = start_probe_timer(ProbeTimeout, Target, NextSequence),
    Probe = #probe{
               target      = Target,
               sequence    = NextSequence,
               ack_timer   = AckTimer,
               probe_timer = ProbeTimer,
               ack_fun     = AckFun
              },
    swim_metrics:notify({probe, Target}),
    NewState#state{probe = Probe, sequence = NextSequence}.

handle_packet(Packet, Peer, State) ->
    case decrypt(Packet, State) of
        {ok, PlainText} ->
            try
                {Message, Events} = swim_messages:decode(PlainText),
                swim_metrics:notify({rx, iolist_size(Packet)}),
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
    #probe{ack_fun = AckFun, ack_timer = AckTimer, probe_timer = ProbeTimer} = Probe,
    AckFun(Responder),
    swim_time:cancel_timer(AckTimer, [{async, true}, {info, false}]),
    swim_time:cancel_timer(ProbeTimer, [{async, true}, {info, false}]),
    State#state{probe = undefined};
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

handle_ack_timeout(Target, Sequence, #state{probe = Probe} = State)
  when Probe#probe.target =:= Target andalso Probe#probe.sequence =:= Sequence ->
    swim_metrics:notify({ack_timeout, Target}),
    swim_time:cancel_timer(Probe#probe.ack_timer, [{async, true}, {info, false}]),
    Msg = {ping_req, Sequence, Probe#probe.target},
    Proxies = swim_state:proxies(Target),
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
    swim_state:probe_timeout(Target, Probe#probe.missing_nacks),
    State#state{probe = undefined};
handle_probe_timeout(_Target, _Sequence, State) ->
    State.

handle_events(Events) ->
    [swim_state:handle_event(Event) || {Category, _} = Event <- Events,
                                       Category =:= membership],
    ok.

% Not sure if we need to handle the case when sending to the socket fails or if we can
% just let it crash.
send({DestIp, DestPort}, Msg, State) ->
    Events = max_broadcasts(),
    Payload = encrypt(swim_messages:encode({Msg, Events}), State),
    ok = swim_socket:send(State#state.socket, DestIp, DestPort, Payload),
    swim_metrics:notify({tx, iolist_size(Payload)}),
    State.

max_broadcasts() ->
    Fold = fun(E, {B, R}) ->
                   EncodedEvent = swim_messages:encode_event(E),
                   case R - iolist_size(EncodedEvent) of
                       L when L > 0 ->
                           {take, {[E | B], L}};
                       _ ->
                           skip
                   end
           end,
    MaxSize = swim_messages:event_size_limit(),
    {Events, _} = swim_state:broadcasts(Fold, {[], MaxSize}),
    Events.

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
