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

-module(prop_swim_messages).

-include_lib("proper/include/proper.hrl").

ip_address() ->
    oneof([
           tuple([integer(0, 255) || _ <- lists:seq(0, 3)]),
           tuple([integer(0, 65535) || _ <- lists:seq(0, 7)])
          ]).

port_number() ->
    integer(0, 65535).

member_status() ->
    oneof([alive, suspect]).

incarnation() ->
    integer(0, inf).

membership_event() ->
    ?LET({Status, Member, Inc},
         {member_status(), member(), incarnation()},
         {membership, {Status, Member, Inc}}).

user_event() ->
    ?LET(Term, term(), {user, Term}).

sequence() ->
    integer(0, inf).

member() ->
    tuple([ip_address(), port_number()]).

swim_event() ->
    oneof([membership_event(), user_event()]).

sized_swim_events() ->
    MaxSize = swim_messages:event_size_limit(),
    ?SIZED(_S, ?LET(Events, list(swim_event()),
                    begin
                        case size(swim_messages:encode_events(Events)) of
                            Size when Size >= MaxSize ->
                                resize(0, list(swim_event()));
                            _ ->
                                Events
                        end
                    end)).

ack() ->
    ?LET({Seq, Target, Events}, {sequence(), member(),
                                 sized_swim_events()},
         swim_messages:encode_ack(Seq, Target, Events)).

ping() ->
    ?LET({Seq, Events}, {sequence(), sized_swim_events()},
         swim_messages:encode_ping(Seq, Events)).

ping_req() ->
    ?LET({Seq, Target}, {sequence(), member()},
         swim_messages:encode_ping_req(Seq, Target)).

swim_message() ->
    oneof([ack(), ping(), ping_req()]).

prop_encode_ack() ->
    ?FORALL({Seq, Target, Events}, {sequence(), member(), list(swim_event())},
            begin
                case events_under_size_limit(Events) of
                    true ->
                        verify_encode_ack(Seq, Target, Events);
                    false ->
                        verify_events_too_large(
                          fun() ->
                                  swim_messages:encode_ack(Seq, Target, Events)
                          end)
                end
            end).

prop_encode_ping() ->
    ?FORALL({Seq, Events}, {sequence(), list(swim_event())},
            begin
                case events_under_size_limit(Events) of
                    true ->
                        verify_encode_ping(Seq, Events);
                    false ->
                        verify_events_too_large(
                          fun() -> swim_messages:encode_ping(Seq, Events) end)
                end
            end).

prop_encode_ping_req() ->
    ?FORALL({Seq, Target}, {sequence(), member()},
            begin
                Encoded = swim_messages:encode_ping_req(Seq, Target),
                {ping_req, MaybeSeq, MaybeTarget} = swim_messages:decode(Encoded),
                MaybeSeq == Seq andalso MaybeTarget == Target
            end).

verify_encode_ping(Seq, Events) ->
    Encoded = swim_messages:encode_ping(Seq, Events),
    {ping, MaybeSeq, MaybeEvents} = swim_messages:decode(Encoded),
    MaybeSeq == Seq andalso
        lists:sort(MaybeEvents) == lists:sort(Events).

key() ->
    binary(32).

mac_key() ->
    non_empty(binary()).

symmetric_keys(G) ->
    ?LET(Key, G, {Key, Key}).

asymmetric_keys(G) ->
    ?LET({Key1, Key2},
         ?SUCHTHAT({K1, K2}, {G, G}, K1 /= K2),
         {Key1, Key2}).

keys(G) ->
    oneof([symmetric_keys(G), asymmetric_keys(G)]).

prop_encryption() ->
    ?FORALL({Keypair, MacKeypair},
            {keys(key()), keys(mac_key())},
            ?FORALL(Message, swim_message(),
                    begin
                        case is_symmetric(Keypair, MacKeypair) of
                            true ->
                                verify_encryption(Keypair, MacKeypair, Message);
                            false ->
                                refute_encryption(Keypair, MacKeypair, Message)
                        end
                    end)).

is_symmetric({Key, Key}, {MacKey, MacKey}) ->
    true;
is_symmetric(_, _) ->
    false.

verify_encryption({Key, Key}, {MacKey, MacKey}, Message) ->
    Encrypted = swim_messages:encrypt(Key, MacKey, Message),
    Message == swim_messages:decrypt(Key, MacKey, Encrypted).

refute_encryption({Key1, Key2}, {MacKey, MacKey}, Message) ->
    Encrypted = swim_messages:encrypt(Key1, MacKey, Message),
    case swim_messages:decrypt(Key2, MacKey, Encrypted) of
        M when M /= Message ->
            true;
        _ ->
            false
    end;
refute_encryption({Key1, Key2}, {MacKey1, MacKey2}, Message) ->
    Encrypted = swim_messages:encrypt(Key1, MacKey1, Message),
    case swim_messages:decrypt(Key2, MacKey2, Encrypted) of
        {error, failed_verification} ->
            true;
        _  ->
            false
    end.

events_under_size_limit(Events) ->
    MaxSize = swim_messages:event_size_limit(),
    Size = size(swim_messages:encode_events(Events)),
    Size =< MaxSize.

verify_encode_ack(Seq, Target, Events) ->
    Encoded = swim_messages:encode_ack(Seq, Target, Events),
    {ack, MaybeSeq, MaybeTarget, MaybeEvents} = swim_messages:decode(Encoded),
    Seq == MaybeSeq andalso
        MaybeTarget == Target andalso
        lists:sort(MaybeEvents) == lists:sort(Events).

verify_events_too_large(Fun) ->
    try
        Fun()
    catch
        error:too_large ->
            true;
        _ ->
            false
    end.
