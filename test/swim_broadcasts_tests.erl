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

-module(swim_broadcasts_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

broadcasts_test_() ->
    {timeout, 60,
     ?_assert(proper:quickcheck(prop_push(), [{to_file, user}]))}.

membership_event() ->
    ?LET(MemberWithStatus, swim_test_utils:member_with_status(),
	 {membership, MemberWithStatus}).

user_event() ->
    ?LET(Event, term(), {user, Event}).

events() ->
    list(oneof([membership_event(), user_event()])).

prop_push() ->
    ?FORALL(Events, events(),
	    begin
		Broadcasts = lists:foldl(
			       fun(Event, Acc) ->
				       swim_broadcasts:push(Event, Acc)
			       end, swim_broadcasts:new(4), Events),
		lists:sort(Events) =:= lists:sort(swim_broadcasts:peek(Broadcasts))
	    end).

prop_pop() ->
    Encode = fun swim_messages:encode_event/1,
    Limit = swim_messages:event_size_limit(),
    ?FORALL(Events, events(),
	    ?FORALL(NumMembers, integer(0, inf),
		    begin
			Broadcasts = lists:foldl(
				       fun(Event, Acc) ->
					       swim_broadcasts:push(Event, Acc)
				       end, swim_broadcasts:new(4), Events),
			{Events, _} = swim_broadcasts:pop(Limit, NumMembers, Encode, Broadcasts),
			size(list_to_binary(Events)) =< Limit
		    end)).
