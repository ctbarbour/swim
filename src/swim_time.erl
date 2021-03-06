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

-module(swim_time).

-export([send_after/3]).
-export([cancel_timer/1]).
-export([cancel_timer/2]).
-export([monotonic_time/0]).

send_after(Time, Dest, Msg) ->
    erlang:send_after(Time, Dest, Msg).

cancel_timer(TRef) ->
    erlang:cancel_timer(TRef).

cancel_timer(TRef, Options) ->
    erlang:cancel_timer(TRef, Options).

monotonic_time() ->
    erlang:monotonic_time().
