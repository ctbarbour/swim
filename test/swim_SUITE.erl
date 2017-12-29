-module(swim_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).

-export([prop_swim_broadcasts/1]).
-export([prop_swim_membership/1]).
-export([prop_swim_keyring/1]).
-export([prop_swim_messages/1]).

all() ->
    [
     prop_swim_membership,
     prop_swim_messages,
     prop_swim_keyring,
     prop_swim_broadcasts
    ].

init_per_suite(Config) ->
    ct_property_test:init_per_suite(Config).

end_per_suite(_Config) ->
    ok.

prop_swim_broadcasts(Config) ->
    ct_property_test:quickcheck(prop_swim_broadcasts:prop_swim_broadcasts(), Config).

prop_swim_membership(Config) ->
    ct_property_test:quickcheck(prop_swim_membership:prop_membership(), Config).

prop_swim_keyring(Config) ->
    ct_property_test:quickcheck(prop_swim_keyring:prop_encryption(), Config).

prop_swim_messages(Config) ->
    ct_property_test:quickcheck(prop_swim_messages:prop_encode_decode(), Config).
