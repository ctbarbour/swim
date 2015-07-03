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

-module(swim_test_utils).

-include_lib("proper/include/proper.hrl").

-export([ip_address/0, port_number/0, member/0, member_with_status/0,
	 member_status/0, incarnation/0, sequence/0,
	 membership_event/0,
	 user_event/0,
	 swim_event/0,
	 ack/0,
	 ping/0,
	 ping_req/0,
	 swim_message/0]).

sequence() ->
    integer(0, inf).

ip_address() ->
    oneof([
	   tuple([integer(0, 255) || _ <- lists:seq(0, 3)]),
	   tuple([integer(0, 65535) || _ <- lists:seq(0, 7)])
	  ]).

port_number() ->
    integer(0, 65535).

member() ->
    tuple([ip_address(), port_number()]).

member_with_status() ->
    tuple([member(), member_status(), incarnation()]).

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
