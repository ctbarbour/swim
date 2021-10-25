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

-module(swim_socket).

-export([connect/5]).
-export([open/2]).
-export([close/1]).
-export([send/4]).
-export([setopts/2]).
-export([listen/3]).
-export([accept/3]).
-export([recv/3]).
-export([send/2]).
-export([peername/1]).

open(Port, Opts) ->
    gen_udp:open(Port, Opts).

connect(tcp, IpAddr, Port, Opts, Timeout) ->
    case gen_tcp:connect(IpAddr, Port, Opts, Timeout) of
        {ok, Socket} ->
            {ok, {tcp, Socket}};
        {error, Reason} ->
            {error, Reason}
    end;
connect(ssl, IpAddr, Port, Opts, Timeout) ->
    case ssl:connect(IpAddr, Port, Opts, Timeout) of
        {ok, Socket} ->
            {ok, {ssl, Socket}};
        {error, Reason} ->
            {error, Reason}
    end.

close({tcp, Socket}) ->
    gen_tcp:close(Socket);
close({ssl, Socket}) ->
    ssl:close(Socket);
close(Socket) ->
    gen_udp:close(Socket).

send(Socket, DestIp, DestPort, Payload) ->
    gen_udp:send(Socket, DestIp, DestPort, Payload).

setopts({tcp, Socket}, Opts) ->
    inet:setopts(Socket, Opts);
setopts({ssl, Socket}, Opts) ->
    ssl:setopts(Socket, Opts);
setopts(Socket, Opts) ->
    inet:setopts(Socket, Opts).

listen(tcp, Port, Opts) ->
    case gen_tcp:listen(Port, Opts) of
        {ok, Socket} ->
            {ok, {tcp, Socket}};
        {error, Reason} ->
            {error, Reason}
    end;
listen(ssl, Port, Opts) ->
    case ssl:listen(Port, Opts) of
        {ok, Socket} ->
            {ok, {ssl, Socket}};
        {error, Reason} ->
            {error, Reason}
    end.

accept({tcp, ListenSocket}, Pid, Timeout) ->
    case gen_tcp:accept(ListenSocket, Timeout) of
        {ok, Socket} ->
            gen_server:cast(Pid, accepted),
            {ok, {tcp, Socket}};
        {error, Reason} ->
            {error, Reason}
    end;
accept({ssl, ListenSocket}, Pid, Timeout) ->
    case ssl:transport_accept(ListenSocket, Timeout) of
        {ok, Socket} ->
            gen_server:cast(Pid, accepted),
            case ssl:handshake(Socket, Timeout) of
                ok ->
                    {ok, {ssl, Socket}};
                {error, closed} ->
                    {error, econnaborted};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

recv({tcp, Socket}, Size, Timeout) ->
    gen_tcp:recv(Socket, Size, Timeout);
recv({ssl, Socket}, Size, Timeout) ->
    ssl:recv(Socket, Size, Timeout).

send({tcp, Socket}, Data) ->
    gen_tcp:send(Socket, Data);
send({ssl, Socket}, Data) ->
    ssl:send(Socket, Data).

peername({tcp, Socket}) ->
    inet:peername(Socket);
peername({ssl, Socket}) ->
    ssl:peername(Socket).
