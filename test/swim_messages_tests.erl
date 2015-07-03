%%% ----------------------------------------------------------------------------
%%% Copyright (c) 2015. All Rights Reserved.
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

-module(swim_messages_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(swim_test_utils, [sequence/0, member/0, swim_event/0, swim_message/0]).

swim_messages_test_() ->
    {timeout, 60, ?_assert(run_props())}.

run_props() ->
    case proper:module(?MODULE, [long_result, {to_file, user}]) of
	[] ->
	    true;
	Result ->
	    ?debugFmt("~p\n", [Result]),
	    false
    end.

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
