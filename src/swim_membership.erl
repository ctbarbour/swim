%%% ----------------------------------------------------------------------------
%%% Copyright (c) 2015-2017. All Rights Reserved.
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

%%% @doc This module is responsible for maintaining the list and status of non
%%% faulty members in a gossip group through the use of the Suspicion Mechanism
%%% described in the SWIM Paper. A `swim_membership' process becomes aware of
%%% membership changes through exported function defined for a specific member
%%% status, {@link alive/3}, {@link suspect/3}, {@link faulty/3}, as determined
%%% by the Failure Detection mechanism of SWIM implemented in {@link swim}. Member
%%% state includes the locally known status of a member as well as a logical clock
%%% for the member's status known in the SWIM paper as the incarnation.
%%% When the status of a member changes events are sent to {@link swim_broadcast}
%%% to be broadcast to the rest of the members in the gossip group.
%%%
%%% @end

-module(swim_membership).

-export([new/3]).
-export([local_member/1]).
-export([members/1]).
-export([size/1]).
-export([alive/3]).
-export([suspect/4]).
-export([faulty/4]).
-export([handle_event/2]).
-export([suspicion_timeout/3]).

-record(membership, {
          local_member             :: swim:member(),
          incarnation        = 0   :: swim:incarnation(),
          members            = #{} :: #{swim:member() := mstate()},
          suspicion_factor         :: pos_integer(),
          protocol_period          :: pos_integer(),
          faulty   = ordsets:new() :: ordsets:ordset(swim:member())
         }).

-record(mstate, {
          status                        :: swim:member_status(),
          incarnation   = 0             :: swim:incarnation(),
          last_modified                 :: integer(),
          suspecting    = ordsets:new() :: ordsets:ordset(swim:member()),
          suspicion_timer               :: undefined | reference()
         }).

-type mstate() :: #mstate{}.
-opaque membership() :: #membership{}.
-export_type([membership/0]).

-spec new(LocalMember, ProtocolPeriod, SuspicionFactor) -> Membership when
      LocalMember     :: swim:member(),
      ProtocolPeriod  :: pos_integer(),
      SuspicionFactor :: pos_integer(),
      Membership      :: membership().

new(LocalMember, ProtocolPeriod, SuspicionFactor) ->
    #membership{
       local_member     = LocalMember,
       protocol_period  = ProtocolPeriod,
       suspicion_factor = SuspicionFactor
      }.

%% @doc The number of known members in the gossip group, including the local member
-spec size(Membership) -> NumMembers when
      Membership :: membership(),
      NumMembers :: non_neg_integer().

size(#membership{members = Members}) ->
    maps:size(Members) + 1.

%% @doc The identifier for the local member
-spec local_member(Membership) -> Member when
      Membership :: membership(),
      Member     :: swim:member().

local_member(#membership{local_member = LocalMember}) ->
    LocalMember.

%% @doc A list of known members and their status
-spec members(Membership) -> [{Member, Status, Incarnation}] when
      Membership  :: membership(),
      Member      :: swim:member(),
      Status      :: swim:member_status(),
      Incarnation :: swim:incarnation().

members(#membership{members = Members}) ->
    maps:fold(
      fun(Member, #mstate{status = Status, incarnation = Incarnation}, Acc) ->
              [{Member, Status, Incarnation} | Acc]
      end, [], Members).

-spec handle_event(Event, Membership0) -> {Events, Membership} when
      Event       :: swim:membership_event(),
      Membership0 :: membership(),
      Events      :: [swim:membership_event()],
      Membership  :: membership().

handle_event({alive, Incarnation, Member}, Membership) ->
    alive(Member, Incarnation, Membership);
handle_event({suspect, Incarnation, Member, From}, Membership) ->
    suspect(Member, Incarnation, From, Membership);
handle_event({faulty, Incarnation, Member, From}, Membership) ->
    faulty(Member, Incarnation, From, Membership).

%% @doc Set the member status to alive
%%
%% If the member isn't known it's added to the membership and an event is
%% broadcast to notify the group of the alive member. If the member is known and the incarnation is
%% greater than the current incarnation of the member we update the incarnation
%% of member and broadcast an event to group. If the member is the local member and the incarnation
%% is greater than the current incarnation we refute the alive message by setting our current
%% incarnation to 1 + the received incarnation and then broad a new alive message to the group.
%% If none of the above conditions are meet we do nothing.
%% @end
-spec alive(Member, Incarnation, Membership0) -> {Events, Membership} when
      Member      :: swim:member(),
      Incarnation :: swim:incarnation(),
      Membership0 :: membership(),
      Events      :: [swim:membership_event()],
      Membership  :: membership().

alive(Member, Incarnation, Membership)
  when Member =:= Membership#membership.local_member andalso
       Incarnation =< Membership#membership.incarnation ->
    {[], Membership};
alive(Member, Incarnation, Membership)
  when Member =:= Membership#membership.local_member andalso
       Incarnation > Membership#membership.incarnation ->
    refute(Incarnation, Membership);
alive(Member, Incarnation, Membership) ->
    #membership{members = CurrentMembers} = Membership,
    case maps:find(Member, CurrentMembers) of
        error ->
            State = #mstate{
              status        = alive,
              incarnation   = Incarnation,
              last_modified = swim_time:monotonic_time()
             },
            NewMembers = maps:put(Member, State, CurrentMembers),
            Events = [{alive, Incarnation, Member}],
            {Events, Membership#membership{members = NewMembers}};
        {ok, OldState}
          when Incarnation > OldState#mstate.incarnation ->
            case OldState#mstate.suspicion_timer of
                undefined -> ok;
                TRef -> swim_time:cancel_timer(TRef)
            end,
            MemberState = #mstate{
              status        = alive,
              incarnation   = Incarnation,
              last_modified = swim_time:monotonic_time()
             },
            NewMembers = maps:put(Member, MemberState, CurrentMembers),
            Events = [{alive, Incarnation, Member}],
            {Events, Membership#membership{members = NewMembers}};
        {ok, _MemberState} ->
            {[], Membership}
    end.

%% @doc Set the member status to suspect
%%
%% If the member isn't already known we do nothing. If the member is known
%% we update the status and broadcast the change on the follow conditions. If
%% the current status of the member is alive and the incarnation is greater than
%% or equal to the known incarnation of the member, we update the member's status
%% to suspect and broadcast the change. If the current status of the member is
%% suspect and the incarnation is greater than the known incarnation, we update
%% the member's status to suspect, set the known incarnation to the provided
%% incarnation and broadcast the change.
%% If the suspected member is the local member we refute by incrementing our own
%% incarnation and broadcasting the change to the group.
%% @end
-spec suspect(Member, Incarnation, From, Membership0) -> {Events, Membership} when
      Member      :: swim:member(),
      Incarnation :: swim:incarnation(),
      From        :: local | swim:member(),
      Membership0 :: membership(),
      Events      :: [swim:membership_event()],
      Membership  :: membership().

suspect(Member, Incarnation, _From, Membership)
  when Member =:= Membership#membership.local_member ->
    refute(Incarnation, Membership);
suspect(Member, Incarnation, local, Membership) ->
    #membership{local_member = From} = Membership,
    suspect(Member, Incarnation, From, Membership);
suspect(Member, Incarnation, From, Membership) ->
    #membership{members = CurrentMembers, local_member = LocalMember} = Membership,
    case maps:find(Member, CurrentMembers) of
        {ok, #mstate{status = suspect} = MemberState}
          when Incarnation > MemberState#mstate.incarnation ->
            NewState = MemberState#mstate{
                         suspecting    = ordsets:add_element(From, MemberState#mstate.suspecting),
                         incarnation   = Incarnation,
                         last_modified = swim_time:monotonic_time()},
            NewMembers = maps:put(Member, NewState, CurrentMembers),
            Events = [{suspect, Incarnation, Member, LocalMember}],
            {Events, Membership#membership{members = NewMembers}};
        {ok, #mstate{status = alive} = MemberState}
          when Incarnation >= MemberState#mstate.incarnation ->
            NewState = MemberState#mstate{
                         status          = suspect,
                         incarnation     = Incarnation,
                         suspecting      = ordsets:add_element(From, MemberState#mstate.suspecting),
                         suspicion_timer = faulty_after(Member, Incarnation, Membership),
                         last_modified   = swim_time:monotonic_time()
                        },
            faulty_after(Member, Incarnation, Membership),
            NewMembers = maps:put(Member, NewState, CurrentMembers),
            Events = [{suspect, Incarnation, Member, LocalMember}],
            {Events, Membership#membership{members = NewMembers}};
        _ ->
            {[], Membership}
    end.

-spec suspicion_timeout(Member, SuspectedAt, Membership0) -> {Events, Membership} when
      Member      :: swim:member(),
      SuspectedAt :: swim:incarnation(),
      Membership0 :: membership(),
      Events      :: [swim:membership_event()],
      Membership  :: membership().

suspicion_timeout(Member, SuspectedAt, Membership) ->
    #membership{members = CurrentMembers, local_member = LocalMember, faulty = Faulty} = Membership,
    {Events, NewMembers} =
        case maps:find(Member, CurrentMembers) of
            {ok, #mstate{incarnation = CurrentIncarnation}}
              when SuspectedAt < CurrentIncarnation ->
                {[], CurrentMembers};
            {ok, #mstate{incarnation = CurrentIncarnation}} ->
                {[{faulty, CurrentIncarnation, Member, LocalMember}],
                 maps:remove(Member, CurrentMembers)};
            _ ->
                {[], CurrentMembers}
        end,
    {Events, Membership#membership{members = NewMembers, faulty = ordsets:add_element(Member, Faulty)}}.

%% @doc Remove the member from the group
%%
%% If the member isn't already known we do nothing. If the member is known
%% we remove the member and broadcast the change if the provided incarnation is
%% greater than the current incarnation of the member.
%% @end
-spec faulty(Member, Incarnation, From, Membership0) -> {Events, Membership} when
      Member      :: swim:member(),
      Incarnation :: swim:incarnation(),
      From        :: local | swim:member(),
      Membership0 :: membership(),
      Events      :: [swim:membership_event()],
      Membership  :: membership().

faulty(Member, Incarnation, _From, Membership)
  when Member =:= Membership#membership.local_member ->
    refute(Incarnation, Membership);
faulty(Member, Incarnation, local, Membership) ->
    #membership{local_member = From} = Membership,
    faulty(Member, Incarnation, From, Membership);
faulty(Member, _Incarnation, _From, Membership) ->
    #membership{members = CurrentMembers, local_member = LocalMember, faulty = Faulty} = Membership,
    case maps:take(Member, CurrentMembers) of
        {#mstate{status = suspect, incarnation = Incarnation}, NewMembers} ->
            {[{faulty, Incarnation, Member, LocalMember}],
             Membership#membership{members = NewMembers,
                                   faulty = ordsets:add_element(Member, Faulty)}};
        _ ->
            {[], Membership}
    end.

%% @private
%% @doc The number of protocol periods between suspecting a member and considering it faulty
faulty_after(Member, Incarnation, Membership) ->
    After = round(math:log(maps:size(Membership#membership.members) + 2)) *
        Membership#membership.suspicion_factor * Membership#membership.protocol_period,
    erlang:send_after(After, self(), {suspicion_timeout, Member, Incarnation}).

%% @private
refute(Incarnation, #membership{local_member = LocalMember} = Membership)
  when Incarnation >= Membership#membership.incarnation ->
    NewIncarnation = Incarnation + 1,
    {[{alive, NewIncarnation, LocalMember}], Membership#membership{incarnation = NewIncarnation}};
refute(Incarnation, #membership{incarnation = CurrentIncarnation} = Membership)
  when Incarnation < CurrentIncarnation ->
    {[{alive, CurrentIncarnation, Membership#membership.local_member}], Membership}.
