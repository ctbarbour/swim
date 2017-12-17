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

-export([encode_ack/3]).
-export([encode_ping/3]).
-export([encode_ping_req/2]).
-export([encode_leave/1]).
-export([encode_event/1]).
-export([encode_events/1]).
-export([encode_member/1]).
-export([decode_events/1]).
-export([decode_event/2]).
-export([decode/1]).
-export([event_size_limit/0]).

-include("swim.hrl").

-define(VERSION_1, 1).
-define(MAX_EVENT_SIZE, 452).

-type msg_code()    ::  1 | 2 | 3 | 4 | 50 | 51 | 100 | 101 | 102.
-type msg_type()    ::  ping | ack | ping_req | leave | membership | user | alive
                      | suspect | faulty.
-type encoded_msg() ::  <<_:8, _:_*8>>.

-spec msg_code(msg_type()) -> msg_code().
msg_code(ping) -> 1;
msg_code(ack) -> 2;
msg_code(ping_req) -> 3;
msg_code(leave) -> 4;
msg_code(membership) -> 50;
msg_code(user) -> 51;
msg_code(alive) -> 100;
msg_code(suspect) -> 101;
msg_code(faulty) -> 102.

-spec msg_type(msg_code()) -> msg_type().
msg_type(1) -> ping;
msg_type(2) -> ack;
msg_type(3) -> ping_req;
msg_type(4) -> leave;
msg_type(50) -> membership;
msg_type(51) -> user;
msg_type(100) -> alive;
msg_type(101) -> suspect;
msg_type(102) -> faulty.

%% @doc Event size limit determines the maximum size (in octets) available to
%%      to piggyback membership and user events on an ACK or PING message.
%%
%% The max message size we use is the minimum reassembly buffer size defined for
%% IPv4 to avoid IP fragmentation -- 576 octets.
%% UDP has and overhead of a 20 octet IP header and an 8 octet
%% UDP header. The max swim message size is 40 octets, with 16 octets for the
%% nonce, and 16 octets for the CipherTag. That leaves 476 octets for the events.
%% A membership event is 41 octets which equates to 11 membership events per
%% ACK/PING message. User messages have an over head of 9 octets, leaving 467
%% octets for the user message payload.
%% @end
-spec event_size_limit() -> ?MAX_EVENT_SIZE.
event_size_limit() ->
    ?MAX_EVENT_SIZE.

%% @doc Encodes an ACK message, piggybacking membership and user events.
%%
%% An ACK message has the following format:
%% <table border='1'>
%%  <tr>
%%    <td>1</td>
%%    <td>4</td>
%%    <td>6</td>
%%    <td>N</td>
%%  </tr>
%%  <tr>
%%    <td>2</td>
%%    <td>Sequence</td>
%%    <td>Member</td>
%%    <td>Events</td>
%%  </tr>
%% </table>
%% <dl>
%%   <dt><strong><code>Sequence</code></strong></dt>
%%   <dd> is the the same Sequence received in the coorisponding PING message</dd>
%%   <dt><strong><code>Member</code></strong></dt>
%%   <dd> is the terminal Member for the coorisponding PING. In the case of
%% a PING-REQ, the Member is not the sender of this ACK.
%% See {@link encode_member/1} for Member encoding.</dd>
%%   <dt><strong><code>Events</code></strong></dt>
%%   <dd> is a list of membership and user events piggybacked as a part of the
%% dissemination protocol.
%% See {@link encode_events/1} for Event encoding.</dd>
%% </dl>
%% @end
-spec encode_ack(sequence(), member(), [swim_event()] | binary())
                -> binary() | no_return().
encode_ack(Seq, Target, Events)
  when is_binary(Events), size(Events) =< ?MAX_EVENT_SIZE ->
    Body = <<(msg_code(ack))/integer,
             Seq:32/integer,
             (encode_member(Target))/binary,
             Events/binary>>,
    encode_envelope(Body);
encode_ack(Seq, Target, Events) when is_list(Events) ->
    encode_ack(Seq, Target, encode_events(Events));
encode_ack(_Seq, _Target, _Events) ->
    error(too_large).

%% @doc Encodes a PING message, piggybacking membership and user events.
%%
%% A PING message has the following format:
%% <table border='1'>
%%   <tr>
%%     <td>1</td>
%%     <td>4</td>
%%     <td>N</td>
%%  </tr>
%%  <tr>
%%    <td>1</td>
%%    <td>Sequence</td>
%%    <td>Events</td>
%%  </tr>
%% </table>
%% @end
-spec encode_ping(Sequence, TargetMember, Events) -> Ping | no_return() when
      Sequence :: sequence(),
      TargetMember :: member(),
      Events :: [swim_event()],
      Ping :: binary().

encode_ping(Seq, TargetMember, Events)
  when is_binary(Events), size(Events) =< ?MAX_EVENT_SIZE ->
    Body = <<(msg_code(ping))/integer,
             Seq:32/integer,
             (encode_member(TargetMember))/binary,
             Events/binary>>,
    encode_envelope(Body);
encode_ping(Seq, TargetMember, Events) when is_list(Events) ->
    encode_ping(Seq, TargetMember, encode_events(Events));
encode_ping(_Seq, _TargetMember, _Events) ->
    error(too_large).

%% @doc Encodes a PING-REQ message.
%%
%% A PING-REQ message has the following format:
%% <table border='1'>
%%   <tr>
%%     <td>1</td>
%%     <td>4</td>
%%     <td>6</td>
%%   </tr>
%%   <tr>
%%     <td>3</td>
%%     <td>Sequence</td>
%%     <td>Member</td>
%%   </tr>
%% </table>
%% <dl>
%%  <dt><strong><code>Sequence</code></strong></dt>
%%  <dd> is the iteration of the failure detection protocol the PING-REQ was
%% sent during</dd>
%%  <dt><strong><code>Member</code></strong></dt>
%%  <dd> the terminal of the PING-REQ. The receiver of the PING-REQ is the proxy
%% for the PING. See {@link encode_member/1} for Member encoding.</dd>
%% </dl>
%% @end
-spec encode_ping_req(sequence(), member()) -> binary().
encode_ping_req(Seq, Target) ->
    Body = <<(msg_code(ping_req))/integer,
             Seq:32/integer,
             (encode_member(Target))/binary>>,
    encode_envelope(Body).

%% @doc Encodes a LEAVE message.
%%
%% <table border='1'>
%%  <tr>
%%    <td>1</td>
%%    <td>4</td>
%%  </tr>
%%  <tr>
%%    <td>4</td>
%%    <td>Sequence</td>
%%  </tr>
%% </table>
%% <dl>
%%   <dt><strong><code>Sequence</code></strong></dt>
%%   <dd>is the iteration of the failure detection protocol the leave message
%% was sent during</dd>
%% </dl>
%% @end
-spec encode_leave(sequence()) -> binary().
encode_leave(Seq) ->
    Body = <<(msg_code(leave))/integer, Seq:32/integer>>,
    encode_envelope(Body).

-spec encode_envelope(encoded_msg()) -> encoded_msg().
encode_envelope(Body) when is_binary(Body) ->
    <<?VERSION_1/integer, Body/binary>>.

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
-spec encode_member(Member::{inet:ip_address(), inet:port_number()})
                   -> <<_:48>> | <<_:96>>.
encode_member({{A1, A2, A3, A4}, Port}) ->
    <<6/integer, A1/integer, A2/integer, A3/integer, A4/integer, Port:16/integer>>;
encode_member({{A1, A2, A3, A4, A5, A6, A7, A8}, Port}) ->
    Address = << <<N:16/integer>> || N <- [A1, A2, A3, A4, A5, A6, A7, A8, Port] >>,
    <<(size(Address))/integer, Address/binary>>.

%% @doc Encodes a list of swim events. See {@link encode_event/1}.
-spec encode_events([swim_event() | binary()]) -> binary().
encode_events(Events) ->
    << <<(encode_event(Event))/binary>> || Event <- Events >>.

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
-spec encode_event(Event::swim_event() | binary()) -> binary().
encode_event({membership, {Status, Member, Inc}}) ->
    Bin = <<(msg_code(Status))/integer,
            (encode_member(Member))/binary,
            Inc:32/integer>>,
    <<(msg_code(membership))/integer, Bin/binary>>;
encode_event({user, Term}) ->
    Bin = term_to_binary(Term),
    <<(msg_code(user))/integer, (size(Bin)):16/integer, Bin/binary>>;
encode_event(Event) when is_binary(Event) ->
    Event.

%% @doc Decodes the provided message from a binary to an Erlang Term.
%%
%% All messages are prefixed with a single octet to indicate the version of
%% of the protocol. The return value is an Erlang term of the message. If the
%% version is not supported or the message is malformed, an exception is thrown.
%% @end
-spec decode(Packet) -> Result when
      Packet :: binary(),
      Result :: swim_message() | error.

decode(<<?VERSION_1/integer, Code/integer, Rest/binary>>) ->
    try
        decode(msg_type(Code), Rest)
    catch
        _ ->
            error
    end.

-spec decode(ack | ping | ping_req | leave, binary()) -> swim_message().
decode(ack, <<Seq:32/integer, MemberSize/integer, Member:MemberSize/binary,
              Events/binary>>) ->
    {ack, Seq, decode_member(Member), decode_events(Events)};
decode(ping, <<Seq:32/integer, MemberSize/integer, Member:MemberSize/binary, Events/binary>>) ->
    {ping, Seq, decode_member(Member), decode_events(Events)};
decode(ping_req, <<Seq:32/integer, MemberSize/integer, Member:MemberSize/binary>>) ->
    {ping_req, Seq, decode_member(Member)};
decode(leave, <<Seq:32/integer>>) ->
    {leave, Seq}.

-spec decode_member(<<_:48, _:_*96>>) -> {inet:ip_address(), inet:port_number()}.
decode_member(<<A1/integer, A2/integer, A3/integer, A4/integer, Port:16/integer>>) ->
    {{A1, A2, A3, A4}, Port};
decode_member(<<A1:16/integer, A2:16/integer, A3:16/integer, A4:16/integer,
                A5:16/integer, A6:16/integer, A7:16/integer, A8:16/integer,
                Port:16/integer>>) ->
    {{A1, A2, A3, A4, A5, A6, A7, A8}, Port}.

-spec decode_events(binary()) -> [swim_event()].
decode_events(Events) ->
    decode_events(Events, []).

-spec decode_events(binary(), [swim_event()]) -> [swim_event()].
decode_events(<<>>, Acc) ->
    Acc;
decode_events(<<Code/integer, Events/binary>>, Acc) ->
    {Event, Rest} = decode_event(msg_type(Code), Events),
    decode_events(Rest, [Event | Acc]).

-spec decode_event(membership | user, binary()) -> {swim_event(), binary()}.
decode_event(membership, <<StatusCode/integer, MemberSize/integer,
                           Member:MemberSize/binary, Inc:32/integer, Rest/binary>>) ->
    Event = {membership, {msg_type(StatusCode), decode_member(Member), Inc}},
    {Event, Rest};
decode_event(user, <<Size:16/integer, Bin:Size/binary, Rest/binary>>) ->
    Event = {user, binary_to_term(Bin)},
    {Event, Rest}.
