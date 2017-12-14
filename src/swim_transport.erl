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
-define(match_message(Msg),
        begin
            receive
                Msg ->
                    true
            after
                1000 ->
                    false
            end
        end).
-endif.

-export([start_link/2]).
-export([ping/5]).
-export([rotate_keys/2]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-define(AAD, crypto:hash(sha256, term_to_binary(erlang:get_cookie()))).

-record(state, {
          local_member      :: member(),
          current_ping      :: undefined | ping(),
          proxy_pings = #{} :: #{member() => member()},
          socket            :: undefined | inet:socket(),
          keys        = []  :: [binary()],
          aad               :: binary()
         }).

start_link(ListenPort, Keys)
  when is_list(Keys) andalso Keys /= [] ->
    gen_server:start_link({local, ?MODULE}, [ListenPort, Keys], []).

rotate_keys(Pid, Key) ->
    gen_server:cast(Pid, {rotate_keys, Key}).

ping(Peer, Incarnation, Sequence, Proxies, Timeout) ->
    gen_server:cast(?MODULE, {send_ping, Peer, Incarnation, Sequence, Proxies, Timeout}).

%% @private
init([ListenPort, Keys]) ->
    LocalMember = swim_state:local_member(),
    SocketOpts = [binary, inet, {active, 16}],
    {ok, Socket} = gen_udp:open(ListenPort, SocketOpts),
    {ok, #state{local_member = LocalMember, keys = Keys, socket = Socket, aad = ?AAD}}.

%% @private
handle_call(_Msg, _From, State) ->
    {noreply, State}.

%% @private
handle_cast({send_ping, Peer, Incarnation, Sequence, Proxies, Timeout}, State) ->
    {noreply, send_ping(Peer, Incarnation, Sequence, Proxies, Timeout, State)};
handle_cast({rotate_keys, Key}, State) ->
    #state{keys=Keys} = State,
    {noreply, State#state{keys=[Key | Keys]}};
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
terminate(_Reason, #state{socket=undefined}) ->
    ok;
terminate(_Reason, #state{socket=Socket}) ->
    gen_udp:close(Socket).

handle_packet(Packet, Peer, State) ->
    #state{keys = Keys, aad = AAD} = State,
    case decrypt(Packet, AAD, Keys) of
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
handle_message({ping, Sequence, _Events}, Peer, State) ->
    handle_ping(Sequence, Peer, State);
handle_message({ping_req, Sequence, Terminal}, Peer, State) ->
    handle_ping_req(Sequence, Terminal, Peer, State).

handle_ping_req(Sequence, Terminal, Origin, State) ->
    Msg = swim_messages:encode_ping(Sequence, []),
    case do_send(Terminal, Msg, State) of
        ok ->
            State#state{proxy_pings = maps:put(Terminal, Origin, State#state.proxy_pings)};
        {error, _Reason} ->
            State
    end.

handle_ping(Sequence, Peer, State) ->
    ok = swim_state:alive(Peer, 0),
    Msg = swim_messages:encode_ack(Sequence, State#state.local_member, []),
    do_send(Peer, Msg, State),
    State.

handle_ack(Sequence, Responder, #state{current_ping = Ping} = State)
  when Ping#ping.sequence =:= Sequence andalso Ping#ping.terminal =:= Responder ->
    time_compat:cancel_timer(Ping#ping.tref),
    ok = swim_state:alive(Responder, Ping#ping.incarnation),
    State#state{current_ping = undefined};
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

handle_ack_timeout(Ref, #state{current_ping = Ping} = State)
  when Ping#ping.ref =:= Ref ->
    [send_ping_req(Proxy, Ping, State) || Proxy <- Ping#ping.proxies],
    State;
handle_ack_timeout(_Ref, State) ->
    State.

send_ping_req(Peer, #ping{terminal = Terminal, sequence = Sequence}, State) ->
    do_send(Peer, swim_message:encode_ping_req(Sequence, Terminal), State).

do_send({DestIp, DestPort}, Msg, State) ->
    Payload = encrypt(Msg, State),
    gen_udp:send(State#state.socket, DestIp, DestPort, Payload).

send_ping({DestIp, DestPort} = Peer, Incarnation, Sequence, Proxies, Timeout, State) ->
    Msg = encrypt(swim_messages:encode_ping(Sequence, []), State),
    case gen_udp:send(State#state.socket, DestIp, DestPort, Msg) of
        ok ->
            Ref = make_ref(),
            TRef = time_compat:send_after(Timeout, self(), {ack_timeout, Ref}),
            Ping = #ping{
                      origin      = State#state.local_member,
                      terminal    = Peer,
                      sequence    = Sequence,
                      proxies     = Proxies,
                      incarnation = Incarnation,
                      tref        = TRef,
                      ref         = Ref
                     },
            State#state{current_ping = Ping};
        {error, _Reason} ->
            State
    end.

encrypt(Msg, State) ->
    #state{keys=[Key|_], aad=AAD} = State,
    swim_messages:encrypt(Key, AAD, Msg).

decrypt(_Data, _AAD, []) ->
    {error, failed_verification};
decrypt(Data, AAD, [Key | Keys]) ->
    case swim_messages:decrypt(Key, AAD, Data) of
        {error, failed_verification} ->
            decrypt(Data, AAD, Keys);
        SwimMessage ->
            {ok, SwimMessage}
    end.

-ifdef(TEST).

local_ipv4() ->
    {ok, Addrs} = inet:getifaddrs(),
    hd([Addr || {_, Opts} <- Addrs,
                {addr, Addr} <- Opts,
                size(Addr) == 4,
                Addr =/= {127,0,0,1}]).
start() ->
    Keys = [crypto:strong_rand_bytes(32)],
    LocalIp = local_ipv4(),
    {ok, Alice} = swim_transport:start_link(LocalIp, 4678, Keys),
    {ok, Bob} = swim_transport:start_link(LocalIp, 4679, Keys),
    #{alice => {Alice, 4678}, bob => {Bob, 4679}, local_ip => LocalIp}.

stop(Config) ->
    #{alice := {Alice, _}, bob := {Bob, _}} = Config,
    [swim_transport:close(P) || P <- [Alice, Bob]],
    ok.

swim_transport_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun(Config) ->
             [send_and_receive_ping(Config),
              send_and_receive_ack(Config),
              send_and_receive_ping_req(Config)]
     end}.

send_and_receive_ping(Config) ->
    #{alice := {Alice, _}, bob := {_, Port}, local_ip := LocalIp} = Config,
    Msg = swim_messages:encode_ping(1, []),
    ok = swim_transport:send(Alice, LocalIp, Port, Msg),
    Result = ?match_message({{ping, 1, []}, _}),
    ?_assert(Result).

send_and_receive_ack(Config) ->
    #{alice := {Alice, _}, bob := {_, Port}, local_ip := LocalIp} = Config,
    Msg = swim_messages:encode_ack(1, {LocalIp, Port}, []),
    ok = swim_transport:send(Alice, LocalIp, Port, Msg),
    Result = ?match_message({{ack, 1, _Responder, []}, _}),
    ?_assert(Result).

send_and_receive_ping_req(Config) ->
    #{alice := {Alice, _}, bob := {_, Port}, local_ip := LocalIp} = Config,
    Msg = swim_messages:encode_ping_req(1, {LocalIp, Port}),
    ok = swim_transport:send(Alice, LocalIp, Port, Msg),
    Result = ?match_message({{ping_req, 1, _Responser}, _}),
    ?_assert(Result).

-endif.
