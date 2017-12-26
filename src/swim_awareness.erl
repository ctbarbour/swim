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

-module(swim_awareness).

-export([new/1]).
-export([success/1]).
-export([failure/1]).
-export([failure/2]).
-export([scale/2]).

-opaque awareness() :: {pos_integer(), non_neg_integer()}.
-export_type([awareness/0]).

-spec new(Max) -> Awareness when
      Max       :: pos_integer(),
      Awareness :: awareness().

new(Max) ->
    {Max, 0}.

-spec success(Awareness0) -> Awareness when
      Awareness0 :: awareness(),
      Awareness  :: awareness().

success({Max, 0}) ->
    {Max, 0};
success({Max, Value}) ->
    {Max, Value - 1}.

-spec failure(Awareness0) -> Awareness when
      Awareness0 :: awareness(),
      Awareness  :: awareness().

failure({Max, Max}) ->
    {Max, Max};
failure({Max, Value}) ->
    {Max, Value + 1}.

-spec failure(N, Awareness0) -> Awareness when
      N          :: pos_integer(),
      Awareness0 :: awareness(),
      Awareness  :: awareness().

failure(N, Awareness) ->
    lists:foldl(fun(_, A) -> failure(A) end, Awareness, lists:seq(1, N)).

-spec scale(Timeout, Awareness) -> Value when
      Timeout   :: non_neg_integer(),
      Awareness :: awareness(),
      Value     :: non_neg_integer().

scale(Timeout, {_Max, Value}) ->
    Timeout * (Value + 1).
