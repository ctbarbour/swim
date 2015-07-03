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

-module(swim_vault_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SERVER, swim_vault).

-behavior(proper_statem).

-export([command/1, initial_state/0, next_state/3, postcondition/3,
	 precondition/2]).

-import(swim_test_utils, [swim_message/0]).

-record(state, {
	  keys      = []  :: list(binary()),
	  plaintext       :: binary(),
	  ciphertext      :: binary()
	 }).

vault_test_() ->
    {timeout, 60,
     ?_assert(proper:quickcheck(prop_vault(), [{to_file, user}]))}.

key() ->
    binary(32).

initial_state() ->
    #state{keys=[]}.

command(S) ->
    frequency([
	       {1, {call, ?SERVER, rotate_keys, [{var, vault}, key()]}},
	       {5, {call, ?SERVER, encrypt, [{var, vault}, swim_message()]}},
	       {5, {call, ?SERVER, decrypt, [{var, vault}, S#state.ciphertext]}}
	      ]).

precondition(S, {call, _, decrypt, _}) ->
    S#state.ciphertext /= undefined andalso S#state.plaintext /= undefined;
precondition(S, {call, _, encrypt, _}) ->
    S#state.plaintext == undefined andalso S#state.ciphertext == undefined;
precondition(_S, {call, _, rotate_keys, _}) ->
    true.

postcondition(S, {call, _, decrypt, _}, {ok, MaybePlaintext}) ->
    #state{plaintext=Plaintext} = S,
    Plaintext == MaybePlaintext;
postcondition(_S, {call, _, rotate_keys, _}, ok) ->
    true;
postcondition(_S, {call, _, encrypt, _}, _R) ->
    true;
postcondition(_S, _Call, _R) ->
    false.

next_state(#state{keys=Keys} = S, _V, {call, _, rotate_keys, [_, Key]}) ->
    S#state{keys=[Key|Keys]};
next_state(S, Ciphertext, {call, _, encrypt, [_, Plaintext]}) ->
    S#state{ciphertext=Ciphertext, plaintext=Plaintext};
next_state(S, _V, {call, _, decrypt, _}) ->
    S#state{ciphertext=undefined, plaintext=undefined}.

prop_vault() ->
    ?FORALL({Cmds, Keys}, {commands(?MODULE), vector(1, key())},
	    ?TRAPEXIT(
	       begin
		   {ok, Pid} = ?SERVER:start_link(Keys),
		   {H, S, R} = run_commands(?MODULE, Cmds, [{vault, Pid}]),
		   ok = ?SERVER:stop(Pid),
		   ?WHENFAIL(
		      io:format("History: ~p\nState: ~p\nResult: ~p\n",
				[H, S, R]),
		      aggregate(command_names(Cmds), R =:= ok))
	       end)).
