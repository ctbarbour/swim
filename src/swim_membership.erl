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

%%% @doc This module is responsible for maintaining the list and status of non
%%% faulty members in a gossip group through the use of the Suspicion Mechanism
%%% described in the SWIM Paper. A `swim_membership' is a data structure. It
%%% should be updated when new members join the gossip group
%%% or drop out as determined by the Failure Detection mechanism of SWIM
%%% implemented as {@link swim_gossip}. A `swim_membership'
%%% further maintains the local gossip peer status, which includes the current
%%% incarnation -- the logical clock for the member status. When a
%%% `swim_membership' is updated via {@link swim_membership:update/3} a list of
%%% membership events is returned indicating changes to the local membership list
%%% which should be broadcast to the rest of the members in the gossip group.
%%%
%%% @end
-module(swim_membership).

-export([new/2,
         members/1,
	 members_with_status/1,
	 is_member/2,
	 status/2,
         size/1,
         alive/2,
         suspect/2,
         leave/2,
         update/3,
         update/4,
         age_members/1
        ]).

-include("swim.hrl").

-record(mbrs, {
	  me                            :: member(),
	  incarnation = 0               :: non_neg_integer(),
	  members     = maps:new()      :: members(),
	  suspicion   = 5               :: age()
	 }).

-record(minfo, {
	  status = alive      :: member_status(),
	  inc    = 0          :: incarnation(),
	  age    = 0          :: age()
	 }).

-type members()          :: #{member() => minfo()}.
-type age()              :: non_neg_integer().
-type minfo()            :: #minfo{}.
-type membership_opt()   :: {suspicion, pos_integer()} | {seeds, [member()]}.

-opaque membership()     :: #mbrs{}.

-export_type([membership/0, membership_opt/0]).

%% @doc Creates a new Membership list.
%%
%% The Membership list requires the owning Member of the Membership
%% list in order to refute any contradictory membership events targeting the
%% owner. We can also provide Members to to populate the Membership list at
%% initalization. Further, we can provide configuration for the suspicion
%% interval.
%% @end
-spec new(member(), [membership_opt()]) -> membership().
new(Me, Opts) ->
    Seeds = proplists:get_value(seeds, Opts, []),
    Suspicion = proplists:get_value(suspicion, Opts, 5),
    InitialMembers = initial_members(Me, Seeds),
    #mbrs{me=Me, members=InitialMembers, suspicion=Suspicion}.

-spec initial_members(member(), [member()]) -> members().
initial_members(Me, Seeds) ->
    lists:foldl(fun(Member, Members) ->
			MemberInfo = #minfo{status=alive, inc=0},
			maps:put(Member, MemberInfo, Members)
		end, maps:new(), [Me|Seeds]).

%% @doc Returns a list of Members -- both alive or suspects.
-spec members(membership()) -> [member()].
members(#mbrs{members=Members}) ->
    maps:keys(
      maps:fold(
	fun(_Member, #minfo{status=faulty}, Map) ->
		Map;
	   (Member, Info, Map) ->
		maps:put(Member, Info, Map)
	end, maps:new(), Members)).

%% @doc Returns the list of Members with their current status
-spec members_with_status(membership()) -> [{member(), member_status()}].
members_with_status(#mbrs{members=Members}) ->
    maps:fold(fun(Member, #minfo{status=Status}, List) ->
		      [{Member, Status} | List]
	      end, [], Members).

%% @doc Returns a boolean indicating whether the Member is in the Membership
%% list or not
-spec is_member(member(), membership()) -> boolean().
is_member(Member, #mbrs{members=Members}) ->
    maps:is_key(Member, Members).

%% @doc Returns the current status of a Member in the local Membership list
-spec status(member(), membership())
	    -> {ok, member_status()} | {error, not_found}.
status(Member, #mbrs{members=Members}) ->
    case maps:find(Member, Members) of
	{ok, #minfo{status=Status}} ->
	    {ok, Status};
	error ->
	    {error, not_found}
    end.

%% @doc Returns the number of Members in the Membership list. Includes Members
%% who are either alive or suspected.
-spec size(membership()) -> non_neg_integer().
size(#mbrs{members=Members}) ->
    maps:size(Members).

%% @doc Update Member's status to Alive using the most recently observed
%% incarnation.
%%
%% @end
-spec alive(member(), membership()) -> {[membership_event()], membership()}.
alive(Member, Mbrs) ->
    update(Member, alive, Mbrs).

%% @doc Suspect a Member
%%
%% Suspecting a Member updates the status of the Member and resets it's age.
%% @end
-spec suspect(member(), membership()) -> {[membership_event()], membership()}.
suspect(Member, Mbrs) ->
    update(Member, suspect, Mbrs).

%% @doc Update Membership to indicate the Member has willingly left.
-spec leave(member(), membership()) -> {[membership_event()], membership()}.
leave(Member, Mbrs) ->
    update(Member, faulty, Mbrs).

%% @doc Increment the age of Members in the Membership list. The
%% gossip protocol should age it's Members on each protocol period to
%% age out suspects.
%%
%% We track Member's age as a useful piece of information to
%% track the churn of Membership. It's also important to track the age
%% of Member's so we can easily age out suspects and mark them as
%% faulty after a number of protcol periods have passed. We use the
%% suspicion multiplier provided on initialization of the
%% Membership. We age out suspects after `round(Suspicion *
%% math:log(NumMembers + 1)' protocol periods have elapsed.
%% @end
-spec age_members(membership()) -> {[membership_event()], membership()}.
age_members(#mbrs{members=Members, suspicion=Suspicion} = Mbrs) ->
    MaxAge = round(Suspicion * math:log(maps:size(Members) + 1)),
    AgedMembership = increment_member_age(Mbrs),
    remove_old_suspects(MaxAge, AgedMembership).

-spec increment_member_age(membership()) -> membership().
increment_member_age(Membership) ->
    #mbrs{members=Members} = Membership,
    NewMembers = maps:map(fun age_member/2, Members),
    Membership#mbrs{members=NewMembers}.

-spec age_member(member(), minfo()) -> minfo().
age_member(_Member, #minfo{age=Age} = Info) ->
    Info#minfo{age=Age + 1}.

-spec remove_old_suspects(age(), membership())
			 -> {[membership_event()], membership()}.
remove_old_suspects(MaxAge, Membership) ->
    #mbrs{members=Members} = Membership,
    {Es, Ms} = maps:fold(fun(Member, Info, {Events, NewMembers}) ->
			   #minfo{status=Status, age=Age, inc=Inc} = Info,
			   case {Status, Age} of
			       {suspect, Age} when Age > MaxAge ->
				   Event = new_membership_event(faulty, Member, Inc),
				   {[Event | Events], NewMembers};
			       _ ->
				   {Events, maps:put(Member, Info, NewMembers)}
			   end
		   end, {[], maps:new()}, Members),
    {Es, Membership#mbrs{members=Ms}}.

%% @doc Update a Member's status using the currently known
%% incarnation.
%%
%% Updating a Member's status using the currently known incarnation is
%% used when we observer a change in a Member's status and would like
%% to broadcast any side-effects to other Members.
%% @end
-spec update(member(), member_status(), membership()) ->
		    {[membership_event()], membership()}.
update(Member, Status, Mbrs) ->
    update(Member, Status, 0, Mbrs).

%% @doc Update a Member's status using a remote incarnation.
%%
%% Updating a Member's status using a remote incarnation is used to
%% override known Membership based on remote observations of a
%% Member's status.
%% @end
-spec update(member(), member_status(), incarnation(), membership())
	    -> {[membership_event()], membership()}.
update(Member, Status, Incarnation, #mbrs{members=Members} = Mbrs) ->
    case {maps:find(Member, Members), Status} of
	{error, alive} ->
	    apply_update(Member, #minfo{status=Status, inc=Incarnation}, Mbrs);
	{error, _Status} ->
	    {[], Mbrs};
	{{ok, Info}, _Status} ->
	    maybe_update({Status, Incarnation}, Info, Member, Mbrs)
    end.

-spec maybe_update({member_status(), incarnation()}, minfo(), member(), membership()) ->
			  {[membership_event()], membership()}.
maybe_update({suspect, I}, #minfo{status=alive, inc=J} = Info, Member, #mbrs{me=Member, incarnation=J} = Mbrs) ->
    refute_change({suspect, I}, Info, Member, Mbrs);
maybe_update({faulty, I}, #minfo{status=alive, inc=J} = Info, Member, #mbrs{me=Member, incarnation=J} = Mbrs) ->
    refute_change({faulty, I}, Info, Member, Mbrs);
maybe_update({alive, I}, #minfo{status=suspect, inc=J} = Info, Member, Mbrs) when I > J ->
    apply_update(Member, Info#minfo{status=alive, inc=I, age=0}, Mbrs);
maybe_update({alive, I}, #minfo{status=alive, inc=J} = Info, Member, Mbrs) when I > J ->
    apply_update(Member, Info#minfo{inc=I, age=0}, Mbrs);
maybe_update({suspect, I}, #minfo{status=suspect, inc=J} = Info, Member, Mbrs) when I > J ->
    apply_update(Member, Info#minfo{inc=I}, Mbrs);
maybe_update({suspect, I}, #minfo{status=alive, inc=J} = Info, Member, Mbrs) when I >= J ->
    apply_update(Member, Info#minfo{status=suspect, inc=I, age=0}, Mbrs);
maybe_update({faulty, I}, #minfo{status=_Status, inc=J} = Info, Member, Mbrs) when I >= J ->
    apply_update(Member, Info#minfo{status=faulty, inc=I}, Mbrs);
maybe_update(_New, _Old, _Member, Mbrs) ->
    {[], Mbrs}.

%% @doc When we receive a Membership event from a remote Member
%% declaring us suspicious or faulty we must refute this status change
%% by broadcasting that we are alive with a larger incarnation.
-spec refute_change({member_status(), incarnation()},
		    minfo(), member(), membership())
		   -> {[membership_event()], membership()}.
refute_change({_Status, I}, #minfo{status=alive, inc=J} = Info, Member, #mbrs{me=Member} = Mbrs) ->
    NewInc = new_incarnation(I, J),
    apply_update(Member, Info#minfo{inc=NewInc}, Mbrs#mbrs{incarnation=NewInc}).

-spec new_incarnation(incarnation(), incarnation()) -> incarnation().
new_incarnation(I, I) ->
    I + 1;
new_incarnation(I, J) when I < J ->
    J.

-spec apply_update(member(), minfo(), membership()) -> {[membership_event()], membership()}.
apply_update(Member, #minfo{status=faulty, inc=Inc}, Mbrs) ->
    #mbrs{members=Members} = Mbrs,
    NewMembers = maps:remove(Member, Members),
    Change = new_membership_event(faulty, Member, Inc),
    {[Change], Mbrs#mbrs{members=NewMembers}};
apply_update(Member, #minfo{status=Status, inc=Inc} = Info, #mbrs{members=Members} = Mbrs) ->
    NewMembers = maps:put(Member, Info, Members),
    Change = new_membership_event(Status, Member, Inc),
    {[Change], Mbrs#mbrs{members=NewMembers}}.

new_membership_event(Status, Member, Inc) ->
    {membership, {Status, Member, Inc}}.
