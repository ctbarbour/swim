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

-module(swim_net).
-behavior(gen_server).

-include("swim.hrl").

-export([start_link/2]).
-export([ping/3]).
-export([ping_req/4]).
-export([ack/2]).
-export([nack/3]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-record(state, {
          local_member      :: member(),
          probes      = #{} :: #{{member(), sequence()} => pid()},
          proxy_pings = #{} :: #{{member(), sequence()} => member()},
          socket            :: undefined | inet:socket(),
          keyring           :: swim_keyring:keyring()
         }).

start_link(ListenPort, Keyring) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ListenPort, Keyring], []).

ping(Target, Origin, Sequence) ->
    gen_server:cast(?MODULE, {ping, self(), Target, Origin, Sequence}).

ping_req(Proxy, Terminal, Origin, Sequence) ->
    gen_server:cast(?MODULE, {ping_req, self(), Proxy, Terminal, Origin, Sequence}).

ack(Target, Sequence) ->
    gen_server:cast(?MODULE, {ack, Target, Sequence}).

nack(Target, Terminal, Sequence) ->
    gen_server:cast(?MODULE, {nack, Target, Terminal, Sequence}).

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
handle_cast({ping, Pid, Terminal, Sequence}, State) ->
    Msg = swim_messages:encode({ping, Sequence, Terminal, []}),
    case send(Terminal, Msg, State) of
        ok ->
            State#state{probes = maps:put({Terminal, Sequence}, Pid, State#state.probes)};
        {error, _Reason} ->
            State
    end;
handle_cast({ping_req, Pid, Proxy, Terminal, Sequence}, State) ->
    Msg = swim_messages:encode({ping_req, Sequence, Terminal, []}),
    _ = send(Proxy, Msg, State),
    State#state{probes = maps:put({Terminal, Sequence}, Pid, State#state.probes)};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({udp_passive, Socket}, #state{socket = Socket} = State) ->
    ok = inet:setopts(Socket, [{active, 16}]),
    {noreply, State};
handle_info({udp, Socket, Ip, InPortNo, Data}, #state{socket=Socket} = State) ->
    {noreply, handle_packet(Data, {Ip, InPortNo}, State)};
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

handle_message({ack, Sequence, Responder, Events}, _Peer, State) ->
    spawn_link(?MODULE, handle_events, [Events]),
    handle_ack(Sequence, Responder, State);
handle_message({nack, Sequence, Responder, Events}, _Peer, State) ->
    spawn_link(?MODULE, handle_events, [Events]),
    handle_nack(Sequence, Responder, State);
handle_message({ping, Sequence, Target, Events}, Peer, State)
  when Target =:= State#state.local_member ->
    spawn_link(?MODULE, handle_events, [Events]),
    handle_ping(Sequence, Peer, State);
handle_message({ping, _Sequence, _Target, _Events}, _Peer, State) ->
    State;
handle_message({ping_req, Sequence, Terminal, Events}, Peer, State) ->
    spawn_link(?MODULE, handle_events, [Events]),
    handle_ping_req(Sequence, Terminal, Peer, State).

handle_ack(Sequence, Responder, State) ->
    case maps:take({Responder, Sequence}, State#state.probes) of
        {Probe, Probes} ->
            ok = swim_probe:ack(Probe, Responder, Sequence),
            State#state{probes = Probes};
        error ->
            case maps:take({Responder, Sequence}, State#state.proxy_pings) of
                {Origin, ProxyPings} ->
                    Msg = swim_messages:encode({ack, Sequence, Responder, []}),
                    case send(Origin, Msg, State) of
                        ok ->
                            State#state{proxy_pings = ProxyPings};
                        {error, _Reason} ->
                            State
                    end;
                error ->
                    State
            end
    end.

handle_nack(Sequence, Responder, State) ->
    case maps:find({Responder, Sequence}, State#state.probes) of
        {ok, Probe} ->
            ok = swim_probe:nack(Probe, Responder, Sequence),
            State;
        error ->
            State
    end.

handle_ping(Sequence, Peer, State) ->
    Msg = swim_messages:encode({ack, Sequence, State#state.local_member, []}),
    _ = send(Peer, Msg, State),
    State.

handle_ping_req(Sequence, Terminal, Origin, State) ->
    Msg = swim_messages:encode({ping, Sequence, Terminal, []}),
    case send(Terminal, Msg, State) of
        ok ->
            State#state{proxy_pings = maps:put({Terminal, Sequence}, Origin, State#state.proxy_pings)};
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
