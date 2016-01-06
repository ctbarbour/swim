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

-module(swim_membership_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SERVER, swim_membership_srv).
-define(ME, {{127,0,0,1}, 5000}).

-behavior(proper_statem).

-export([command/1, initial_state/0, next_state/3, postcondition/3,
	 precondition/2]).

-type age()            :: non_neg_integer().
-type member()         :: {inet:ip_address(), inet:port_number()}.
-type member_status()  :: alive | suspect | faulty.
-type incarnation()    :: non_neg_integer().
-type membership()     :: {member(), member_status(), incarnation(), age()}.

-record(state, {
	  me           :: member(),
	  members = [] :: list(membership())
	 }).

%% -----------------
%% EUnit test runner
%% -----------------

membership_test_() ->
    {timeout, 60,
     ?_assert(proper:quickcheck(prop_membership(), [{numtests, 300},
						    {to_file, user}]))}.

%% -----------------
%% PropEr Generators
%% -----------------

existing_member(#state{me=Me, members=Members}) ->
    ?LET({Member, Status, Inc, _Age},
	 ?SUCHTHAT({M, _, _, _}, oneof(Members), M /= Me),
	 {Member, member_status(Status), incarnation(Inc)}).

update_existing_member(#state{me=Me, members=Members} = S) ->
    HasMembers = (lists:keydelete(Me, 1, Members) /= []),
    frequency([{2, ?LET(Member,
			exactly(Me),
			{Member, oneof([suspect, faulty]), 0})}] ++
		  [{3, existing_member(S)} || HasMembers]).

call_update(S) ->
    ?LET({Member, Status, Inc},
	 frequency([
		    {3, update_existing_member(S)},
		    {2, swim_test_utils:member_with_status()}
		   ]),
	 {call, ?SERVER, update, [Member, Status, Inc]}).

member_status(Current) ->
    ?LET(Status,
	 ?SUCHTHAT(S, oneof([alive, suspect, faulty]), S /= Current), Status).

incarnation(Current) ->
    oneof([Current, Current + 1]).

%% -----------------------
%% PropEr StateM callbacks
%% -----------------------

initial_state() ->
    #state{me=?ME, members=[{?ME, alive, 0, 0}]}.

command(S) ->
    oneof([{call, ?SERVER, members_with_status, []},
	   {call, ?SERVER, age_members, []},
	   call_update(S)
	  ]).

precondition(_State, _Call) ->
    true.

postcondition(State, {call, _, members_with_status, _}, MembersWithStatus) ->
    Local = lists:map(fun({Member, Status, _Inc, _Age}) ->
			      {Member, Status}
		      end, State#state.members),
    lists:sort(MembersWithStatus) == lists:sort(Local);
postcondition(#state{me=Me}, {call, _, update, [Me, _Status, Inc]}, Events) ->
    lists:member({membership, {alive, Me, Inc + 1}}, Events);
postcondition(S, {call, _, update, [Member, NewStatus, NewInc]}, Events) ->
    #state{members=Members} = S,
    case lists:keyfind(Member, 1, Members) of
	{Member, CurrentStatus, CurrentInc, _Age} ->
	    case {CurrentStatus, NewStatus} of
		{suspect, alive} when NewInc > CurrentInc ->
		    Event = {membership, {NewStatus, Member, NewInc}},
		    lists:member(Event, Events);
		{alive, suspect} when NewInc >= CurrentInc ->
		    Event = {membership, {NewStatus, Member, NewInc}},
		    lists:member(Event, Events);
		{_Status, faulty} when NewInc >= CurrentInc ->
		    Event = {membership, {faulty, Member, NewInc}},
		    lists:member(Event, Events);
		{Status, Status} when NewInc > CurrentInc ->
		    Event = {membership, {Status, Member, NewInc}},
		    lists:member(Event, Events);
		_ ->
		    true
	    end;
	false ->
	    case NewStatus of
		alive ->
		    Event = {membership, {NewStatus, Member, NewInc}},
		    lists:member(Event, Events);
		_ ->
		    true
	    end
    end;
postcondition(_State, {call, _, age_members, _}, _Events) ->
    true.

next_state(#state{me=Me} = S, _V, {call, _, update, [Me, _Status, _Inc]}) ->
    #state{members=Members} = S,
    {Me, alive, Inc, Age} = lists:keyfind(Me, 1, Members),
    NewMember = {Me, alive, Inc + 1, Age},
    S#state{members=lists:keyreplace(Me, 1, Members, NewMember)};
next_state(S, _V, {call, _, update, [Member, NewStatus, NewInc]}) ->
    #state{members=Members} = S,
    case lists:keyfind(Member, 1, Members) of
	{Member, CurrentStatus, CurrentInc, Age} ->
	    NewMembers =
		case {CurrentStatus, NewStatus} of
		    {suspect, alive} when NewInc > CurrentInc ->
			lists:keyreplace(Member, 1, Members,
					 {Member, NewStatus, NewInc, 0});
		    {alive, suspect} when NewInc >= CurrentInc ->
			lists:keyreplace(Member, 1, Members,
					 {Member, NewStatus, NewInc, 0});
		    {_Status, faulty} when NewInc >= CurrentInc ->
			lists:keydelete(Member, 1, Members);
		    {Status, Status} when NewInc > CurrentInc ->
			lists:keyreplace(Member, 1, Members,
					 {Member, Status, NewInc, Age});
		    _ ->
			Members
		end,
	    S#state{members=NewMembers};
	false when NewStatus =:= alive ->
	    S#state{members=[{Member, NewStatus, NewInc, 0} | Members]};
	false  ->
	    S
    end;
next_state(S, _V, {call, _, age_members, []}) ->
    #state{members=Members} = S,
    MaxAge = round(5 * math:log(length(Members) + 1)),
    YoungMembers = lists:filtermap(
		     fun({Member, Status, Inc, Age}) ->
			     case {Status, Age} of
				 {suspect, Age} when Age >= MaxAge ->
				     false;
				 _ ->
				     {true, {Member, Status, Inc, Age + 1}}
			     end
		     end, Members),
    S#state{members=YoungMembers};
next_state(S, _V, _Call) ->
    S.

%% -------------------------------
%% PropEr swim_membership property
%% -------------------------------

prop_membership() ->
    ?FORALL(Cmds, commands(?MODULE),
	    ?TRAPEXIT(
	       begin
		   {ok, _Pid} = ?SERVER:start_link(?ME, []),
		   {H, S, R} = run_commands(?MODULE, Cmds),
		   ok = ?SERVER:stop(),
		   ?WHENFAIL(
		      io:format("History: ~p\nState: ~p\nResult: ~p\n",
				[H, S, R]),
		      aggregate(command_names(Cmds), R =:= ok))
	       end)).
