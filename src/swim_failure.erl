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

-export([start_link/2]).
-export([probe/3]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-record(state, {
          local_member      :: member(),
          probe             :: undefined | probe(),
          ping_reqs = #{}   :: #{{member(), sequence()} => member()},
          ping_req_timeout  :: timeout(),
          socket            :: undefined | inet:socket(),
          keyring           :: swim_keyring:keyring()
         }).

-record(probe, {
          target    :: member(),
          sequence  :: sequence(),
          ack_timer :: reference()
         }).

-type probe() :: #probe{}.

start_link(ListenPort, Keyring) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ListenPort, Keyring], []).

probe(Target, Sequence, Timeout) ->
    gen_server:cast(?MODULE, {probe, Target, Sequence, Timeout}).

%% @private
init([ListenPort, Keyring, PingReqTimeout]) ->
    LocalMember = swim_state:local_member(),
    SocketOpts = [binary, inet, {active, 16}],
    {ok, Socket} = gen_udp:open(ListenPort, SocketOpts),
    State = #state{
               local_member     = LocalMember,
               keyring          = Keyring,
               socket           = Socket,
               ping_req_timeout = PingReqTimeout
              },
    {ok, State}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

%% @private
handle_cast({probe, Target, Sequence, AckTimeout}, State) ->
    Msg = swim_messages:encode({ping, Sequence, Target, []}),
    case send(Target, Msg, State) of
        ok ->
            AckTimer = swim_time:send_after(AckTimeout, self(), ack_timeout),
            Probe = #probe{
                       target    = Target,
                       sequence  = Sequence,
                       ack_timer = AckTimer
                      },
            State#state{probe = Probe};
        {error, _Reason} ->
            State
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({udp_passive, Socket}, #state{socket = Socket} = State) ->
    ok = inet:setopts(Socket, [{active, 16}]),
    {noreply, State};
handle_info({udp, Socket, Ip, InPortNo, Packet}, #state{socket = Socket} = State) ->
    {noreply, handle_packet(Packet, {Ip, InPortNo}, State)};
handle_info(ack_timeout, #state{probe = undefined} = State) ->
    {noreply, State};
handle_info(ack_timeout, #state{probe = Probe} = State) ->
    _ = swim_time:cancel_timer(Probe#probe.ack_timer),
    Msg = swim_messages:encode({ping_req, Probe#probe.sequence, Probe#probe.target, []}),
    [send(Proxy, Msg, State) || Proxy <- swim_state:proxies(Probe#probe.target)],
    {noreply, State};
handle_info({ping_req_timeout, Terminal, Sequence}, State) ->
    case maps:find(Terminal, State#state.ping_reqs) of
        {ok, Origin} ->
            Msg = swim_messages:encode({nack, Sequence, Origin, []}),
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
  when Responder =:= Probe#probe.target ->
    ok = swim_state:ack(Responder, Sequence),
    State#state{probe = undefined};
handle_ack(Sequence, Responder, State) ->
    case maps:take(Responder, State#state.ping_reqs) of
        {Origin, ProxyPings} ->
            Msg = swim_messages:encode({ack, Sequence, Responder, []}),
            ok = send(Origin, Msg, State),
            State#state{ping_reqs = ProxyPings};
        error ->
            State
    end.

handle_nack(Sequence, Responder, #state{probe = Probe} = State)
  when Responder =:= Probe#probe.target ->
    ok = swim_state:nack(Responder, Sequence),
    State;
handle_nack(_Sequence, _Responder, State) ->
    State.

handle_ping(Target, Sequence, Peer, #state{local_member = Target} = State) ->
    Msg = swim_messages:encode({ack, Sequence, Target, []}),
    _ = send(Peer, Msg, State),
    State;
handle_ping(_Target, _Sequence, _Peer, State) ->
    State.

handle_ping_req(Sequence, Terminal, Origin, State) ->
    Msg = swim_messages:encode({ping, Sequence, Terminal, []}),
    case send(Terminal, Msg, State) of
        ok ->
            TimeoutContext = {ping_req_timeout, Terminal, Sequence},
            _TRef = swim_time:send_after(State#state.ping_req_timeout, self(), TimeoutContext),
            State#state{ping_reqs = maps:put(Terminal, Origin, State#state.ping_reqs)};
        {error, _Reason} ->
            State
    end.

send({DestIp, DestPort}, Msg, State) ->
    Payload = encrypt(Msg, State),
    gen_udp:send(State#state.socket, DestIp, DestPort, Payload).

encrypt(Msg, State) ->
    swim_keyring:encrypt(Msg, State#state.keyring).

decrypt(CipherText, State) ->
    swim_keyring:decrypt(CipherText, State#state.keyring).
