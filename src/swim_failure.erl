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

-include("swim.hrl").

-export([start_link/4]).
-export([probe/2]).
-export([probe/4]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-record(state, {
          local_member       :: member(),
          probe              :: undefined | probe(),
          ping_reqs    = #{} :: #{{member(), sequence()} => {member(), sequence()}},
          nack_timeout       :: non_neg_integer(),
          ack_timeout        :: non_neg_integer(),
          socket             :: undefined | inet:socket(),
          keyring            :: swim_keyring:keyring(),
          sequence     = 0   :: sequence()
         }).

-record(probe, {
          target    :: member(),
          sequence  :: sequence(),
          ack_timer :: reference(),
          ack_fun   :: fun((member()) -> ok),
          nack_fun  :: fun((member()) -> ok)
         }).

-type probe() :: #probe{}.

start_link(ListenPort, Keyring, AckTimeout, NackTimeout) ->
    Args = [ListenPort, Keyring, AckTimeout, NackTimeout],
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

probe(Target, Timeout) ->
    AckFun = fun swim_state:ack/1,
    NackFun = fun swim_state:nack/1,
    probe(Target, Timeout, AckFun, NackFun).

probe(Target, Timeout, AckFun, NackFun) ->
    Msg = {probe, Target, Timeout, AckFun, NackFun},
    gen_server:cast(?MODULE, Msg).

%% @private
init([ListenPort, Keyring, AckTimeout, NackTimeout]) ->
    LocalMember = swim_state:local_member(),
    SocketOpts = [binary, inet, {active, 16}],
    {ok, Socket} = gen_udp:open(ListenPort, SocketOpts),
    State = #state{
               local_member     = LocalMember,
               keyring          = Keyring,
               socket           = Socket,
               nack_timeout     = NackTimeout,
               ack_timeout      = AckTimeout
              },
    {ok, State}.

%% @private
handle_call(_Msg, _From, State) ->
    {noreply, State}.

%% @private
handle_cast({probe, Target, AckTimeout, AckFun, NackFun}, State)
  when State#state.probe =:= undefined ->
    NextSequence = State#state.sequence + 1,
    Msg = {ping, NextSequence, Target, []},
    case send(Target, Msg, State) of
        ok ->
            AckTimer = swim_time:send_after(AckTimeout, self(), ack_timeout),
            Probe = #probe{
                       target    = Target,
                       sequence  = NextSequence,
                       ack_timer = AckTimer,
                       ack_fun   = AckFun,
                       nack_fun  = NackFun
                      },
            {noreply, State#state{probe = Probe, sequence = NextSequence}};
        {error, _Reason} ->
            {noreply, State}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({udp_passive, Socket}, #state{socket = Socket} = State) ->
    ok = inet:setopts(Socket, [{active, 16}]),
    {noreply, State};
handle_info({udp, Socket, Ip, InPortNo, Packet}, #state{socket = Socket} = State) ->
    {noreply, handle_packet(Packet, {Ip, InPortNo}, State)};
handle_info({ack_timeout, Terminal, Sequence}, #state{probe = undefined} = State) ->
    case maps:take({Terminal, Sequence}, State#state.ping_reqs) of
        {{_Origin, _PingReqSequence}, PingReqs} ->
            {noreply, State#state{ping_reqs = PingReqs}};
        error ->
            {noreply, State}
    end;
handle_info({ack_timeout, Target, Sequence}, #state{probe = Probe} = State)
  when Probe#probe.target =:= Target andalso Probe#probe.sequence =:= Sequence ->
    _ = swim_time:cancel_timer(Probe#probe.ack_timer),
    Msg = {ping_req, Sequence, Probe#probe.target, []},
    [send(Proxy, Msg, State) || Proxy <- swim_state:proxies(Target)],
    {noreply, State};
handle_info({ack_timeout, _Target, _Sequence}, State) ->
    {noreply, State};
handle_info({nack_timeout, Target, Sequence}, State) ->
    case maps:find({Target, Sequence}, State#state.ping_reqs) of
        {ok, {Origin, PingReqSequence}} ->
            Msg = {nack, PingReqSequence, Origin, []},
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

handle_packet(Packet, Peer, State) ->
    case decrypt(Packet, State) of
        {ok, PlainText} ->
            try
                Message = swim_messages:decode(PlainText),
                handle_message(Message, Peer, State)
            catch
                _:_ ->
                    State
            end;
        {error, failed_verification} ->
            State
    end.

handle_message({ack, Sequence, Terminal, _Events}, _Peer, State) ->
    handle_ack(Sequence, Terminal, State);
handle_message({nack, Sequence, Terminal, _Events}, _Peer, State) ->
    handle_nack(Sequence, Terminal, State);
handle_message({ping, Sequence, Target, _Events}, Peer, State) ->
    handle_ping(Target, Sequence, Peer, State);
handle_message({ping_req, Sequence, Terminal, _Events}, Peer, State) ->
    handle_ping_req(Sequence, Terminal, Peer, State).

handle_ack(Sequence, Responder, #state{probe = Probe} = State)
  when Responder =:= Probe#probe.target andalso Probe#probe.sequence =:= Sequence ->
    #probe{ack_fun = AckFun} = Probe,
    ok = AckFun(Responder),
    State#state{probe = undefined};
handle_ack(Sequence, Responder, State) ->
    case maps:take(Responder, State#state.ping_reqs) of
        {Origin, ProxyPings} ->
            Msg = {ack, Sequence, Responder, []},
            ok = send(Origin, Msg, State),
            State#state{ping_reqs = ProxyPings};
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
    Msg = {ack, Sequence, Target, []},
    _ = send(Peer, Msg, State),
    State;
handle_ping(_Target, _Sequence, _Peer, State) ->
    State.

handle_ping_req(PingReqSequence, Terminal, Origin, State) ->
    NextSequence = State#state.sequence + 1,
    Msg = {ping, NextSequence, Terminal, []},
    case send(Terminal, Msg, State) of
        ok ->
            start_ack_timer(State#state.ack_timeout, Terminal, NextSequence),
            start_nack_timer(State#state.nack_timeout, Terminal, NextSequence),
            PingReqs = maps:put({Terminal, NextSequence}, {Origin, PingReqSequence},
                                State#state.ping_reqs),
            State#state{ping_reqs = PingReqs, sequence = NextSequence};
        {error, _Reason} ->
            State
    end.

send({DestIp, DestPort}, Msg, State) ->
    Payload = encrypt(swim_messages:encode(Msg), State),
    gen_udp:send(State#state.socket, DestIp, DestPort, Payload).

start_ack_timer(Timeout, Terminal, Sequence) ->
    swim_time:send_after(Timeout, self(), {ack_timeout, Terminal, Sequence}).

start_nack_timer(Timeout, Terminal, Sequence) ->
    swim_time:send_after(Timeout, self(), {nack_timeout, Terminal, Sequence}).

encrypt(Msg, State) ->
    swim_keyring:encrypt(Msg, State#state.keyring).

decrypt(CipherText, State) ->
    swim_keyring:decrypt(CipherText, State#state.keyring).
