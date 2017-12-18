%%% ----------------------------------------------------------------------------
%%% Copyright (c) 2015-2017. All Rights Reserved.
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

%%% @doc This module is responsible for encoding and decoding SWIM protocol
%%% messages.
%%%
%%% SWIM protocol message encodings can be found in the documentation
%%% cooresponding to the various encoding functions defined in this module.
%%% {@link encode_ack/3}, {@link encode_ping/3}, {@link encode_ping_req/2},
%%% {@link encode_leave/1}.
%%% All SWIM protocol messages are prefixed with a single octet reflecting
%%% the protocol version of the message. The overall format of SWIM messages is:
%%%
%%% <table border='1'>
%%%   <tr>
%%%     <td>1</td>
%%%     <td>1</td>
%%%     <td>N</td>
%%%  </tr>
%%%  <tr>
%%%    <td>Version</td>
%%%    <td>Tag</td>
%%%    <td>Data</td>
%%%  </tr>
%%% </table>
%%% - __*Version*__ : is the protocol version the message is encoded for
%%% - __*Tag*__ : indicates what type of SWIM message Data represents; ACK, PING,
%%%               PING-REG, or LEAVE
%%% - __*Data*__ : The SWIM messages payload
%%%
%%% All SWIM messages are encrypted over the wire using AES128-GCM. See
%%% {@link swim_keyring:encrypt/2} for more information.
%%% @end
-module(swim_messages).

-export([encode/1]).
-export([decode/1]).
-export([event_size_limit/0]).

-include("swim.hrl").

-define(HEADER, 1).

%% @doc Event size limit determines the maximum size (in octets) available to
%%      to piggyback membership and user events on an ACK or PING message.
%%
%% The max message size we use is the minimum reassembly buffer size defined for
%% IPv4 to avoid IP fragmentation -- 576 octets.
%% UDP has an overhead of a 20 octet IP header and an 8 octet
%% UDP header. The max swim message size is 40 octets, with 16 octets for the
%% nonce, and 16 octets for the CipherTag. That leaves 476 octets for the events.
%% A membership event is 41 octets which equates to 11 membership events per
%% ACK/PING/PING-REQ message. User messages have an over head of 9 octets, leaving 467
%% octets for the user message payload.
%% @end
-spec event_size_limit() -> non_neg_integer().
event_size_limit() ->
    452.

encode({ack, Sequence, Member, Events}) ->
    [?HEADER, $a, <<Sequence:32/integer>>, encode_member(Member), encode_events(Events)];
encode({nack, Sequence, Member, Events}) ->
    [?HEADER, $n, <<Sequence:32/integer>>, encode_member(Member), encode_events(Events)];
encode({ping, Sequence, Member, Events}) ->
    [?HEADER, $p, <<Sequence:32/integer>>, encode_member(Member), encode_events(Events)];
encode({ping_req, Sequence, Member, Events}) ->
    [?HEADER, $r, <<Sequence:32/integer>>, encode_member(Member), encode_events(Events)].

encode_events(Events) ->
    L = length(Events),
    [<<L:8/integer>>, encode_es(Events)].

encode_es([]) ->
    [];
encode_es([Event | Events]) ->
    [encode_event(Event) | encode_es(Events)].

%% @doc Encode either a membership event or a user event.
%%
%% A membership event is encoded as follows:
%% <table border='1'>
%%  <tr>
%%    <td>1</td>
%%    <td>1</td>
%%    <td>6</td>
%%    <td>4</td>
%%  </tr>
%%  <tr>
%%    <td>50</td>
%%    <td>Status</td>
%%    <td>Member</td>
%%    <td>Incarnation</td>
%%  </tr>
%% </table>
%% <dl>
%%   <dt><strong><code>Status</code></strong></dt>
%%   <dd>is observed status of the Member being broadcast to the group</dd>
%%   <dt><strong><code>Member</code></strong></dt>
%%   <dd>is the subject of this membership event</dd>
%%   <dt><strong><code>Incarnation</code></strong></dt>
%%   <dd>is the incarnation of the subject Member known by the sender of this
%% event. See {@link swim_membership} for more information on Incarnations.</dd>
%% </dl>
%%
%% A user event is encoded as follows:
%% <table border='1'>
%%   <tr>
%%     <td>1</td>
%%     <td>2</td>
%%     <td>Size</td>
%%   </tr>
%%   <tr>
%%     <td>51</td>
%%     <td>Size</td>
%%     <td>Erlang Term</td>
%%   </tr>
%% </table>
%% @end
-spec encode_event(Event) -> iolist() when
      Event :: swim_event() | binary().

encode_event({membership, {Status, Incarnation, Member}}) ->
    Code = case Status of
               alive   -> $a;
               suspect -> $s;
               faulty  -> $f
           end,
    [$m, Code, <<Incarnation:32/integer>>, encode_member(Member)];
encode_event({user, Term}) ->
    Bin = term_to_binary(Term),
    [$u, <<(byte_size(Bin)):16/integer>>, Bin].

%% @doc Encodes a Member as the IP address and port number combination.
%%
%% <table border='1'>
%%   <tr>
%%     <td>1</td>
%%     <td>Size</td>
%%     <td>2</td>
%%   </tr>
%%   <tr>
%%     <td>Size</td>
%%     <td>IP Address</td>
%%     <td>Port Number</td>
%%   </tr>
%% </table>
%% <dl>
%%   <dt><strong><code>IP Address</code></strong></dt>
%%   <dd>is the IPv4 or IPv6 address the Member can be reached</dd>
%%   <dt><strong><code>Port Number</code></strong></dt>
%%   <dd>is the associated Port Number the Member is listening on</dd>
%% </dl>
%% @end
-spec encode_member(Member) -> iolist() when
      Member :: member().

encode_member({{A1, A2, A3, A4}, Port}) ->
    <<6, A1:8/integer, A2:8/integer, A3:8/integer, A4:8/integer, Port:16/integer>>;
encode_member({{A1, A2, A3, A4, A5, A6, A7, A8}, Port}) ->
    <<18,
      A1:16/integer, A2:16/integer, A3:16/integer, A4:16/integer,
      A5:16/integer, A6:16/integer, A7:16/integer, A8:16/integer,
      Port:16/integer>>.

%% @doc Decodes the provided message from a binary to an Erlang Term.
%%
%% All messages are prefixed with a single octet to indicate the version of
%% of the protocol. The return value is an Erlang term of the message. If the
%% version is not supported or the message is malformed, an exception is thrown.
%% @end
-spec decode(Packet) -> Result when
      Packet :: binary(),
      Result :: swim_message() | no_return().

decode(<<?HEADER/integer, $a, Sequence:32/integer, L:8, Member:L/binary, Events/binary>>) ->
    {ack, Sequence, decode_member(Member), decode_events(Events)};
decode(<<?HEADER/integer, $n, Sequence:32/integer, L:8, Member:L/binary, Events/binary>>) ->
    {nack, Sequence, decode_member(Member), decode_events(Events)};
decode(<<?HEADER/integer, $r, Sequence:32/integer, L:8, Member:L/binary, Events/binary>>) ->
    {ping_req, Sequence, decode_member(Member), decode_events(Events)};
decode(<<?HEADER/integer, $p, Sequence:32/integer, L:8, Member:L/binary, Events/binary>>) ->
    {ping, Sequence, decode_member(Member), decode_events(Events)}.

decode_member(<<A1/integer, A2/integer, A3/integer, A4/integer, Port:16/integer>>) ->
    {{A1, A2, A3, A4}, Port};
decode_member(<<A1:16/integer, A2:16/integer, A3:16/integer, A4:16/integer,
                A5:16/integer, A6:16/integer, A7:16/integer, A8:16/integer,
                Port:16/integer>>) ->
    {{A1, A2, A3, A4, A5, A6, A7, A8}, Port}.

decode_events(<<L:8, Events/binary>>) ->
    decode_es(L, Events).

decode_es(0, <<>>) ->
    [];
decode_es(K, <<$m, StatusCode:8/integer, Inc:32/integer, L:8, Member:L/binary, Events/binary>>) ->
    Status = case StatusCode of
                 $a -> alive;
                 $s -> suspect;
                 $f -> faulty
             end,
    [{membership, {Status, Inc, decode_member(Member)}} | decode_es(K - 1, Events)];
decode_es(K, <<$u, L:16/integer, Event:L, Events>>) ->
    [{user, binary_to_term(Event)} | decode_es(K - 1, Events)].
