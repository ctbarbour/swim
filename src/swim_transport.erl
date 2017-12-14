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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(match_message(Msg), begin
                                receive
                                    Msg ->
                                        true
                                after
                                    1000 ->
                                        false
                                end
                            end).
-endif.

-export([start_link/3]).
-export([send/4]).
-export([close/1]).
-export([rotate_keys/2]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-record(state, {
          parent    :: pid(),
          socket    :: undefined | inet:socket(),
          keys = [] :: [binary()],
          aad       :: binary()
         }).

start_link(ListenIp, ListenPort, Keys)
  when is_list(Keys) andalso Keys /= [] ->
    gen_server:start_link(?MODULE, [self(), ListenIp, ListenPort, Keys], []).

close(Pid) ->
    gen_server:call(Pid, close).

rotate_keys(Pid, Key) ->
    gen_server:cast(Pid, {rotate_keys, Key}).

send(Pid, DestIp, DestPort, Data) ->
    gen_server:cast(Pid, {send, DestIp, DestPort, Data}).

init([Parent, ListenIp, ListenPort, Keys]) ->
    SocketOpts = [binary, {ip, ListenIp}, {active, once}],
    {ok, Socket} = gen_udp:open(ListenPort, SocketOpts),
    {ok, #state{parent=Parent, keys=Keys, socket=Socket, aad=aad()}}.

handle_call(close, _From, State) ->
    #state{socket=Socket} = State,
    ok = gen_udp:close(Socket),
    {stop, normal, ok, State#state{socket=undefined}};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({rotate_keys, Key}, State) ->
    #state{keys=Keys} = State,
    {noreply, State#state{keys=[Key | Keys]}};
handle_cast({send, DestIp, DestPort, Msg}, State) ->
    #state{socket=Socket} = State,
    Encrypted = encrypt(Msg, State),
    ok = gen_udp:send(Socket, DestIp, DestPort, Encrypted),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({udp, Socket, Ip, InPortNo, Data}, #state{socket=Socket} = State) ->
    NewState = handle_udp(Data, {Ip, InPortNo}, State),
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state{socket=undefined}) ->
    ok;
terminate(_Reason, #state{socket=Socket}) ->
    ok = gen_udp:close(Socket),
    ok.

handle_udp(Data, From, State) ->
    #state{parent=Parent, keys=Keys, aad=AAD} = State,
    case decrypt(Data, AAD, Keys) of
        {ok, SwimMessage} ->
            case catch(swim_messages:decode(SwimMessage)) of
                {'EXIT', _Reason} ->
                    State;
                DecodedMessage ->
                    Parent ! {DecodedMessage, From},
                    State
            end;
        {error, failed_verification} ->
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

aad() ->
    crypto:hash(sha256, term_to_binary(erlang:get_cookie())).

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
