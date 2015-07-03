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

%%% @copyright 2015
%%% @version {@version}

%%% @doc This module is responsible for maintaining the secure keys used for
%%% encrypting and decrypted SWIM protocol messages.
-module(swim_vault).
-behavior(gen_server).

-include("swim.hrl").

-export([start_link/1, encrypt/2, decrypt/2, rotate_keys/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).

-record(state, {
	  keys = [] :: [key()],
	  aad       :: binary()
	 }).

-spec start_link([key()]) -> {ok, pid()}.
start_link(Keys) when Keys /= [] ->
    gen_server:start_link(?MODULE, [Keys], []).

%% @doc Encrypt the provided plain text using the latest key. For information on
%% how data encrypted see {@link swim_messages:encrypt/3}.
-spec encrypt(atom() | pid(), binary()) -> binary().
encrypt(Name, Msg) ->
    gen_server:call(Name, {encrypt, Msg}).

%% @doc Decrypt and authenticate the provided cipher text using the known keys.
%% We attempt to use the most recently provide and continue attempts until we
%% have exhausted all known keys.
-spec decrypt(atom() | pid(), binary())
	     -> {ok, binary()} | {error, failed_verification}.
decrypt(Name, Data) ->
    gen_server:call(Name, {decrypt, Data}).

%% @doc Add a new key to the rotation. This key will now be used as the key for
%% {@link encrypt/1}.
-spec rotate_keys(atom() | pid(), key()) -> ok.
rotate_keys(Name, Key) ->
    gen_server:cast(Name, {rotate_keys, Key}).

-spec stop(atom() | pid()) -> ok.
stop(Name) ->
    gen_server:call(Name, stop).

init([Keys]) ->
    {ok, #state{keys=Keys, aad=aad()}}.

handle_call({encrypt, Msg}, _From, State) ->
    #state{keys=[Key|_], aad=AAD} = State,
    {reply, swim_messages:encrypt(Key, AAD, Msg), State};
handle_call({decrypt, Data}, _From, State) ->
    #state{keys=Keys, aad=AAD} = State,
    {reply, decrypt(Data, AAD, Keys), State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({rotate_keys, Key}, State) ->
    #state{keys=Keys} = State,
    {noreply, State#state{keys=[Key | Keys]}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

-spec aad() -> binary().
aad() ->
    crypto:hash(sha256, term_to_binary(erlang:get_cookie())).

-spec decrypt(binary(), binary(), [key()])
	     -> {ok, swim_message()} | {error, failed_verification}.
decrypt(_Data, _AAD, []) ->
    {error, failed_verification};
decrypt(Data, AAD, [Key | Keys]) ->
    case swim_messages:decrypt(Key, AAD, Data) of
	{error, failed_verification} ->
	    decrypt(Data, AAD, Keys);
	SwimMessage ->
	    {ok, SwimMessage}
    end.
