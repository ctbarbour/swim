-module(prop_swim_keyring).

-include_lib("proper/include/proper.hrl").

-export([prop_encryption/0]).

-import(swim_generators, [swim_message/0]).

g_key() ->
    binary(32).

g_symmetric_keys() ->
    ?LET(Key, g_key(), {swim_keyring:new([Key]), swim_keyring:new([Key])}).

g_asymmetric_keys() ->
    ?LET({Key1, Key2},
         ?SUCHTHAT({K1, K2}, {g_key(), g_key()}, K1 /= K2),
         {swim_keyring:new([Key1]), swim_keyring:new([Key2])}).

g_keypair() ->
    oneof([g_symmetric_keys(), g_asymmetric_keys()]).

g_encoded_message() ->
    ?LET(Message, swim_message(),
         iolist_to_binary(swim_messages:encode(Message))).

prop_encryption() ->
    ?FORALL(Keypair, g_keypair(),
            ?FORALL(Message, g_encoded_message(),
                    begin
                        case is_symmetric(Keypair) of
                            true ->
                                aggregate([symmetric],
                                          assert_encryption(Keypair, Message));
                            false ->
                                aggregate([asynmetric],
                                          refute_encryption(Keypair, Message))
                        end
                    end)).

is_symmetric({Key, Key}) ->
    true;
is_symmetric(_) ->
    false.

assert_encryption({Key1, Key2}, Message) ->
    Encrypted = swim_keyring:encrypt(Message, Key1),
    case swim_keyring:decrypt(Encrypted, Key2) of
        {ok, Message} ->
            true;
        _ ->
            false
    end.

refute_encryption({Key1, Key2}, Message) ->
    Encrypted = swim_keyring:encrypt(Message, Key1),
    case swim_keyring:decrypt(Encrypted, Key2) of
        {error, failed_verification} ->
            true;
        _ ->
            false
    end.
