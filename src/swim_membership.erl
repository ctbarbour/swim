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

-include("swim.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([new/3]).
-export([local_member/1]).
-export([members/1]).
-export([size/1]).
-export([alive/3]).
-export([suspect/2]).
-export([suspect/3]).
-export([confirm/2]).
-export([confirm/3]).
-export([suspicion_timeout/3]).

-record(membership, {
          local_member             :: member(),
          incarnation        = 0   :: incarnation(),
          members            = #{} :: #{member() => member_state()},
          suspicion_factor         :: pos_integer(),
          protocol_period          :: pos_integer()
         }).

-type member_state() ::
        #{
           status        => member_state(),
           incarnation   => incarnation(),
           last_modified => erlang:timestamp(),
           fault_after   => incarnation()
         }.
-opaque membership() :: #membership{}.
-export_type([membership/0]).

-spec new(LocalMember, ProtocolPeriod, SuspicionFactor) -> Membership when
      LocalMember     :: member(),
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
      Member     :: member().

local_member(#membership{local_member = LocalMember}) ->
    LocalMember.

%% @doc A list of known members and their status
-spec members(Membership) -> [{Member, Status, Incarnation}] when
      Membership  :: membership(),
      Member      :: member(),
      Status      :: member_status(),
      Incarnation :: incarnation().

members(#membership{members = Members}) ->
    maps:fold(
      fun(Member, #{status := Status, incarnation := Incarnation}, Acc) ->
              [{Member, Status, Incarnation} | Acc]
      end, [], Members).

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
      Member      :: member(),
      Incarnation :: incarnation(),
      Membership0 :: membership(),
      Events      :: [membership_event()],
      Membership  :: membership().

alive(Member, Incarnation, Membership)
  when Member =:= Membership#membership.local_member andalso
       Incarnation =< Membership#membership.incarnation ->
    Membership;
alive(Member, Incarnation, Membership)
  when Member =:= Membership#membership.local_member andalso
       Incarnation > Membership#membership.incarnation ->
    refute(Incarnation, Membership);
alive(Member, Incarnation, Membership) ->
    #membership{members = CurrentMembers} = Membership,
    case maps:find(Member, CurrentMembers) of
        error ->
            State = #{
              status        => alive,
              incarnation   => Incarnation,
              last_modified => swim_time:monotonic_time()
             },
            NewMembers = maps:put(Member, State, CurrentMembers),
            Events = [{alive, Member, Incarnation}],
            {Events, Membership#membership{members = NewMembers}};
        {ok, #{incarnation := CurrentIncarnation} = S}
          when Incarnation > CurrentIncarnation ->
            case maps:find(tref, S) of
                {ok, TRef} -> swim_time:cancel_timer(TRef);
                error -> ok
            end,
            State = #{
              status        => alive,
              incarnation   => Incarnation,
              last_modified => swim_time:monotonic_time()
             },
            NewMembers = maps:put(Member, State, CurrentMembers),
            Events = [{alive, Member, Incarnation}],
            {Events, Membership#membership{members = NewMembers}};
        {ok, _State} ->
            {[], Membership}
    end.

suspect(Member, Membership) ->
    suspect(Member, current, Membership).

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
-spec suspect(Member, Incarnation, Membership0) -> {Events, Membership} when
      Member      :: member(),
      Incarnation :: incarnation(),
      Membership0 :: membership(),
      Events      :: [membership_event()],
      Membership  :: membership().

suspect(Member, Incarnation, Membership)
  when Member =:= Membership#membership.local_member ->
    refute(Incarnation, Membership);
suspect(Member, Incarnation, Membership) ->
    #membership{members = CurrentMembers} = Membership,
    case maps:find(Member, CurrentMembers) of
        {ok, #{status := suspect, incarnation := CurrentIncarnation} = MemberState}
          when Incarnation =:= current orelse Incarnation > CurrentIncarnation ->
            NewIncarnation = case Incarnation of current -> CurrentIncarnation;
                                 _ -> Incarnation end,
            NewState = MemberState#{
                         incarnation   => NewIncarnation,
                         last_modified => swim_time:monotonic_time()},
            NewMembers = maps:put(Member, NewState, CurrentMembers),
            Events = [{suspect, Member, Incarnation}],
            {Events, Membership#membership{members = NewMembers}};
        {ok, #{status := alive, incarnation := CurrentIncarnation} = MemberState}
          when Incarnation =:= current orelse Incarnation >= CurrentIncarnation ->
            NewIncarnation = case Incarnation of current -> CurrentIncarnation;
                                 _ -> Incarnation end,
            NewState = MemberState#{
                         status        => suspect,
                         incarnation   => NewIncarnation,
                         tref          => confirm_after(Member, NewIncarnation, Membership),
                         last_modified => swim_time:monotonic_time()
                        },
            confirm_after(Member, NewIncarnation, Membership),
            NewMembers = maps:put(Member, NewState, CurrentMembers),
            Events = [{suspect, Member, Incarnation}],
            {Events, Membership#membership{members = NewMembers}};
        error ->
            {[], Membership}
    end.

suspicion_timeout(Member, SuspectedAt, Membership) ->
    case maps:find(Member, Membership#membership.members) of
        {ok, #{incarnation := CurrentIncarnation}}
          when SuspectedAt =< CurrentIncarnation ->
            confirm(Member, Membership);
        _ ->
            {[], Membership}
    end.

confirm(Member, Membership) ->
    confirm(Member, current, Membership).

%% @doc Remove the member from the group
%%
%% If the member isn't already known we do nothing. If the member is known
%% we remove the member and broadcast the change if the provided incarnation is
%% greater than the current incarnation of the member.
%% @end
-spec confirm(Member, Incarnation, Membership0) -> {Events, Membership} when
      Member      :: member(),
      Incarnation :: incarnation(),
      Membership0 :: membership(),
      Events      :: [membership_event()],
      Membership  :: membership().

confirm(Member, Incarnation, Membership)
  when Member =:= Membership#membership.local_member andalso
       is_integer(Incarnation) andalso Incarnation >= 0 ->
    refute(Incarnation, Membership);
confirm(Member, current, Membership) ->
    #membership{members = CurrentMembers} = Membership,
    case maps:take(Member, CurrentMembers) of
        {#{status := suspect, incarnation := Incarnation}, NewMembers} ->
            {[{confirm, Member, Incarnation}],
             Membership#membership{members = NewMembers}};
        _ ->
            {[], Membership}
    end;
confirm(Member, Incarnation, Membership)
  when is_integer(Incarnation) andalso Incarnation >= 0 ->
    #membership{members = CurrentMembers} = Membership,
    {Events, NewMembers} =
        case maps:find(Member, CurrentMembers) of
            {ok, #{incarnation := CurrentIncarnation}}
              when Incarnation < CurrentIncarnation ->
                {[], CurrentMembers};
            {ok, #{incarnation := CurrentIncarnation}} ->
                {[{confirm, Member, CurrentIncarnation}], maps:remove(Member, CurrentMembers)};
            _ ->
                {[], CurrentMembers}
        end,
    {Events, Membership#membership{members = NewMembers}}.

%% @private
%% @doc The number of protocol periods between suspecting a member and considering it faulty
confirm_after(Member, Incarnation, Membership) ->
    After = round(math:log(maps:size(Membership#membership.members) + 2)) *
        Membership#membership.suspicion_factor * Membership#membership.protocol_period,
    erlang:send_after(After, self(), {suspicion_timeout, Member, Incarnation}).

%% @private
refute(Incarnation, #membership{local_member = LocalMember} = Membership)
  when Incarnation >= Membership#membership.incarnation ->
    NewIncarnation = Incarnation + 1,
    {[{alive, LocalMember, NewIncarnation}], Membership#membership{incarnation = NewIncarnation}};
refute(Incarnation, #membership{incarnation = CurrentIncarnation} = Membership)
  when Incarnation < CurrentIncarnation ->
    {[{alive, Membership#membership.local_member, CurrentIncarnation}], Membership}.
