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

-module(swim_transport).
-behavior(gen_server).

-include("swim.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start_link/2]).
-export([probe/5]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-record(state, {
          local_member      :: member(),
          current_probe     :: undefined | probe(),
          proxy_pings = #{} :: #{member() => member()},
          socket            :: undefined | inet:socket(),
          keyring           :: swim_keyring:keyring()
         }).

-record(probe, {
          sequence    :: non_neg_integer(),
          origin      :: member(),
          terminal    :: member(),
          incarnation :: incarnation(),
          proxies     :: nonempty_list(member()),
          ref         :: undefined | reference(),
          tref        :: undefined | reference()
         }).

-type probe() :: #probe{}.

start_link(ListenPort, Keyring) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ListenPort, Keyring], []).

probe(Peer, Incarnation, Sequence, Proxies, Timeout) ->
    gen_server:cast(?MODULE, {send_probe, Peer, Incarnation, Sequence, Proxies, Timeout}).

%% @private
init([ListenPort, Keyring]) ->
    LocalMember = swim_state:local_member(),
    SocketOpts = [binary, inet, {active, 16}],
    {ok, Socket} = gen_udp:open(ListenPort, SocketOpts),
    {ok, #state{local_member = LocalMember, keyring = Keyring, socket = Socket}}.

%% @private
handle_call(_Msg, _From, State) ->
    {noreply, State}.

%% @private
handle_cast({send_probe, Peer, Incarnation, Sequence, Proxies, Timeout}, State) ->
    {noreply, start_probe(Peer, Incarnation, Sequence, Proxies, Timeout, State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({udp_passive, Socket}, #state{socket = Socket} = State) ->
    ok = inet:setopts(Socket, [{active, 16}]),
    {noreply, State};
handle_info({udp, Socket, Ip, InPortNo, Data}, #state{socket=Socket} = State) ->
    {noreply, handle_packet(Data, {Ip, InPortNo}, State)};
handle_info({ack_timeout, Ref}, State) ->
    {noreply, handle_ack_timeout(Ref, State)};
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
            case swim_messages:decode(PlainText) of
                error ->
                    State;
                Message ->
                    handle_message(Message, Peer, State)
            end;
        {error, failed_verification} ->
            State
    end.

handle_message({ack, Sequence, Responder, _Events}, _Peer, State) ->
    handle_ack(Sequence, Responder, State);
handle_message({ping, Sequence, Target, _Events}, Peer, State)
  when Target =:= State#state.local_member ->
    handle_ping(Sequence, Peer, State);
handle_message({ping, _Sequence, Target, _Events}, _Peer, State) ->
    State;
handle_message({ping_req, Sequence, Terminal}, Peer, State) ->
    handle_ping_req(Sequence, Terminal, Peer, State).

handle_ping_req(Sequence, Terminal, Origin, State) ->
    Msg = swim_messages:encode_ping(Sequence, Terminal, []),
    case do_send(Terminal, Msg, State) of
        ok ->
            State#state{proxy_pings = maps:put(Terminal, Origin, State#state.proxy_pings)};
        {error, _Reason} ->
            State
    end.

handle_ping(Sequence, Peer, State) ->
    ok = swim_state:ping(Peer),
    Msg = swim_messages:encode_ack(Sequence, State#state.local_member, []),
    do_send(Peer, Msg, State),
    State.

handle_ack(Sequence, Responder, #state{current_probe = Probe} = State)
  when Probe#probe.sequence =:= Sequence andalso Probe#probe.terminal =:= Responder ->
    swim_time:cancel_timer(Probe#probe.tref),
    ok = swim_state:ack(Responder, Probe#probe.incarnation),
    State#state{current_probe = undefined};
handle_ack(Sequence, Responder, State) ->
    case maps:take(Responder, State#state.proxy_pings) of
        {Origin, ProxyPings} ->
            Msg = swim_messages:encode_ack(Sequence, Responder, []),
            case do_send(Origin, Msg, State) of
                ok ->
                    State#state{proxy_pings = ProxyPings};
                {error, _Reason} ->
                    State
            end;
        error ->
            State
    end.

handle_ack_timeout(Ref, #state{current_probe = Probe} = State)
  when Probe#probe.ref =:= Ref ->
    [send_ping_req(Proxy, Probe, State) || Proxy <- Probe#probe.proxies],
    State;
handle_ack_timeout(_Ref, State) ->
    State.

send_ping_req(Peer, #probe{terminal = Terminal, sequence = Sequence}, State) ->
    do_send(Peer, swim_messages:encode_ping_req(Sequence, Terminal), State).

do_send({DestIp, DestPort}, Msg, State) ->
    Payload = encrypt(Msg, State),
    gen_udp:send(State#state.socket, DestIp, DestPort, Payload).

start_probe({DestIp, DestPort} = Peer, Incarnation, Sequence, Proxies, Timeout, State) ->
    Msg = encrypt(swim_messages:encode_ping(Sequence, []), State),
    case gen_udp:send(State#state.socket, DestIp, DestPort, Msg) of
        ok ->
            Ref = make_ref(),
            TRef = swim_time:send_after(Timeout, self(), {ack_timeout, Ref}),
            Probe = #probe{
                      origin      = State#state.local_member,
                      terminal    = Peer,
                      sequence    = Sequence,
                      proxies     = Proxies,
                      incarnation = Incarnation,
                      tref        = TRef,
                      ref         = Ref
                     },
            State#state{current_probe = Probe};
        {error, _Reason} ->
            State
    end.

encrypt(Msg, State) ->
    swim_keyring:encrypt(Msg, State#state.keyring).

decrypt(CipherText, State) ->
    swim_keyring:decrypt(CipherText, State#state.keyring).
