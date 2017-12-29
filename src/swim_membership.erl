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

-export([new/5]).
-export([local_member/1]).
-export([local_state/1]).
-export([members/1]).
-export([probe_target/1]).
-export([proxies/3]).
-export([size/1]).
-export([refuted/2]).
-export([alive/3]).
-export([suspect/4]).
-export([faulty/4]).
-export([handle_event/2]).

-record(membership, {
          local_member                      :: swim:member(),
          incarnation       = 0             :: swim:incarnation(),
          members           = #{}           :: #{swim:member() := state()},
          faulty            = ordsets:new() :: ordsets:ordset(swim:member()),
          probe_targets     = []            :: [swim:member()],
          alpha                             :: pos_integer(),
          beta                              :: pos_integer(),
          protocol_period                   :: pos_integer(),
          suspicion_factor                  :: pos_integer()
         }).

-record(alive, {
          incarnation = 0 :: swim:incarnation(),
          last_modified   :: integer()
         }).

-record(suspect, {
          incarnation                :: swim:incarnation(),
          suspecting = ordsets:new() :: ordsets:ordset(swim:member()),
          tref                       :: reference(),
          last_modified              :: integer(),
          min                        :: float(),
          max                        :: float(),
          k                          :: non_neg_integer(),
          timeout                    :: pos_integer()
         }).

-type state()   :: alive() | suspect().
-type alive()   :: #alive{}.
-type suspect() :: #suspect{}.

-opaque membership() :: #membership{}.
-export_type([membership/0]).

new(LocalMember, Alpha, Beta, ProtocolPeriod, SuspicionFactor) ->
    #membership{
       local_member     = LocalMember,
       alpha            = Alpha,
       beta             = Beta,
       protocol_period  = ProtocolPeriod,
       suspicion_factor = SuspicionFactor
      }.

%% @doc The number of known members in the gossip group, including the local member
-spec size(Membership) -> NumMembers when
      Membership :: membership(),
      NumMembers :: non_neg_integer().

size(#membership{members = Members}) ->
    maps:size(Members) + 1.

-spec members(Membership) -> Members when
      Membership :: membership(),
      Members    :: [{swim:member(), alive | suspect, swim:incarnation()}].

members(#membership{members = Members}) ->
    maps:fold(fun(Member, #alive{incarnation = Inc}, Acc) ->
                      [{Member, alive, Inc} | Acc];
                 (Member, #suspect{incarnation = Inc}, Acc) ->
                      [{Member, suspect, Inc} | Acc]
              end, [], Members).

%% @doc The identifier for the local member
-spec local_member(Membership) -> Member when
      Membership :: membership(),
      Member     :: swim:member().

local_member(#membership{local_member = LocalMember}) ->
    LocalMember.

-spec local_state(Membership) -> Events when
      Membership :: membership(),
      Events     :: [swim:membership_event()].

local_state(#membership{local_member = LocalMember, incarnation = Inc, members = Members}) ->
    [{membership, {alive, Inc, LocalMember}} |
     maps:fold(
       fun(Member, #suspect{incarnation = Incarnation}, Acc) ->
               [{membership, {suspect, Incarnation, Member, LocalMember}} | Acc];
          (Member, #alive{incarnation = Incarnation}, Acc) ->
               [{membership, {alive, Incarnation, Member}} | Acc]
       end, [], Members)].

-spec probe_target(Membership0) -> none | {Target, Membership} when
      Membership0 :: membership(),
      Target      :: {swim:member(), swim:incarnation()},
      Membership  :: membership().

probe_target(#membership{probe_targets = []} = Membership)
  when map_size(Membership#membership.members) =:= 0 ->
    none;
probe_target(#membership{probe_targets = []} = Membership) ->
    Members = maps:keys(Membership#membership.members),
    Targets = [M || {_, M} <- lists:keysort(1, [{rand:uniform(), N} || N <- Members])],
    probe_target(Membership#membership{probe_targets = Targets});
probe_target(#membership{probe_targets = [T | Targets]} = Membership) ->
    Target =
        case maps:find(T, Membership#membership.members) of
            {ok, #alive{incarnation = Inc}} -> {T, Inc};
            {ok, #suspect{incarnation = Inc}} -> {T, Inc};
            error -> probe_target(Membership#membership{probe_targets = Targets})
        end,
    {Target, Membership#membership{probe_targets = Targets}}.

-spec proxies(Num, Target, Membership) -> Proxies when
      Num        :: pos_integer(),
      Target     :: swim:member(),
      Membership :: membership(),
      Proxies    :: [swim:member()].

proxies(Num, Target, Membership) ->
    Members = maps:keys(Membership#membership.members),
    Targets = [M || {_, M} <- lists:keysort(1, [{rand:uniform(), N} || N <- Members]), M =/= Target],
    lists:sublist(Targets, Num).

-spec handle_event(Event, Membership0) -> {Events, Membership} when
      Event       :: swim:membership_event(),
      Membership0 :: membership(),
      Events      :: [swim:membership_event()],
      Membership  :: membership().

handle_event({membership, {alive, Incarnation, Member}}, Membership) ->
    alive(Member, Incarnation, Membership);
handle_event({membership, {suspect, Incarnation, Member, From}}, Membership) ->
    suspect(Member, Incarnation, From, Membership);
handle_event({membership, {faulty, Incarnation, Member, From}}, Membership) ->
    faulty(Member, Incarnation, From, Membership);
handle_event(_Event, Membership) ->
    {[], Membership}.

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
    #membership{members = CurrentMembers, faulty = Faulty} = Membership,
    case maps:find(Member, CurrentMembers) of
        error ->
            State = #alive{incarnation = Incarnation,
                           last_modified = swim_time:monotonic_time()},
            ProbeTargets = Membership#membership.probe_targets ++ [Member],
            Events = [{membership, {alive, Incarnation, Member}}],
            NewMembers = maps:put(Member, State, CurrentMembers),
            {Events, Membership#membership{
                       members = NewMembers,
                       probe_targets = ProbeTargets,
                       faulty = ordsets:del_element(Member, Faulty)}};
        {ok, #suspect{} = Suspect}
          when Incarnation > Suspect#suspect.incarnation ->
            swim_time:cancel_timer(Suspect#suspect.tref, [{async, true}, {info, false}]),
            Alive = #alive{
              incarnation   = Incarnation,
              last_modified = swim_time:monotonic_time()
             },
            NewMembers = maps:put(Member, Alive, CurrentMembers),
            Events = [{membership, {alive, Incarnation, Member}}],
            {Events, Membership#membership{members = NewMembers}};
        {ok, #alive{incarnation = CurrentInc} = Alive0}
          when Incarnation > CurrentInc ->
            Alive = Alive0#alive{incarnation = Incarnation,
                                 last_modified = swim_time:monotonic_time()},
            {[], Membership#membership{members = maps:put(Member, Alive, CurrentMembers)}};
        {ok, _} ->
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
        {ok, #suspect{suspecting = Suspecting, incarnation = CurrentIncarnation, k = K} = Suspect}
          when Incarnation >= CurrentIncarnation ->
            case {ordsets:is_element(From, Suspecting), ordsets:size(Suspecting) < K} of
                {false, true} ->
                    Elapsed = swim_time:cancel_timer(Suspect#suspect.tref),
                    Timeout = remaining_suspicion_time(Elapsed, Suspect),
                    TRef = start_timer(Timeout, Member, Incarnation),
                    NewState = Suspect#suspect{
                                 suspecting    = ordsets:add_element(From, Suspecting),
                                 incarnation   = Incarnation,
                                 tref          = TRef,
                                 last_modified = swim_time:monotonic_time(),
                                 timeout       = Timeout},
                    NewMembers = maps:put(Member, NewState, CurrentMembers),
                    Events = [{membership, {suspect, Incarnation, Member, From}}],
                    {Events, Membership#membership{members = NewMembers}};
                _ ->
                    NewState = Suspect#suspect{incarnation = Incarnation,
                                               last_modified = swim_time:monotonic_time()},
                    NewMembers = maps:put(Member, NewState, CurrentMembers),
                    {[], Membership#membership{members = NewMembers}}
            end;
        {ok, #alive{incarnation = CurrentIncarnation}}
          when Incarnation >= CurrentIncarnation ->
            {Min, Max, K, Timeout} = initial_suspicion_timeout(Membership),
            TRef = start_timer(Timeout, Member, Incarnation),
            NewState = #suspect{
                          incarnation   = Incarnation,
                          suspecting    = ordsets:from_list([From]),
                          tref          = TRef,
                          last_modified = swim_time:monotonic_time(),
                          min           = Min,
                          max           = Max,
                          k             = K,
                          timeout       = Timeout
                        },
            NewMembers = maps:put(Member, NewState, CurrentMembers),
            Events = [{membership, {suspect, Incarnation, Member, LocalMember}}],
            {Events, Membership#membership{members = NewMembers}};
        _ ->
            {[], Membership}
    end.

start_timer(Timeout, Member, Incarnation) ->
    swim_time:send_after(Timeout, self(), {suspicion_timeout, Member, Incarnation}).

remaining_suspicion_time(Remaining, Suspect) ->
    #suspect{suspecting = Suspecting, k = K, min = Min, max = Max, timeout = Total} = Suspect,
    Elapsed = Total - Remaining,
    Frac = math:log(ordsets:size(Suspecting) + 1) / math:log(K + 1),
    Timeout = floor(max(Min, Max - (Max - Min) * Frac)),
    Timeout - Elapsed.

initial_suspicion_timeout(Membership) ->
    N = maps:size(Membership#membership.members),
    Min = Membership#membership.alpha * max(1, math:log(N)) * Membership#membership.protocol_period,
    Max = Membership#membership.beta * Min,
    % If there aren't enough members in the group excluding ourselves and the suspected member we
    % won't expect any additional suspicions so we immediately set the timeout to Min.
    K = case N < Membership#membership.suspicion_factor - 2 of
            true -> 0;
            false -> Membership#membership.suspicion_factor
        end,
    Timeout = case K < 1 of
                  true -> Min;
                  false -> Max
              end,
    {Min, Max, K, floor(Timeout)}.

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
faulty(Member, Incarnation, From, Membership) ->
    #membership{members = CurrentMembers, faulty = Faulty} = Membership,
    case maps:find(Member, CurrentMembers) of
        {ok, #suspect{incarnation = CurrentIncarnation}}
          when Incarnation >= CurrentIncarnation ->
            {[{membership, {faulty, Incarnation, Member, From}}],
             Membership#membership{members = maps:remove(Member, CurrentMembers),
                                   faulty = ordsets:add_element(Member, Faulty)}};
        _ ->
            {[], Membership}
    end.

refuted([], _Membership) ->
    false;
refuted([{membership, {alive, _Inc, Member}} | _Events], Membership)
  when Membership#membership.local_member =:= Member ->
    true;
refuted([_Event | Events], Membership) ->
    refuted(Events, Membership).

%% @private
refute(Incarnation, #membership{local_member = LocalMember} = Membership)
  when Incarnation >= Membership#membership.incarnation ->
    NewIncarnation = Incarnation + 1,
    {[{membership, {alive, NewIncarnation, LocalMember}}],
     Membership#membership{incarnation = NewIncarnation}};
refute(Incarnation, #membership{incarnation = CurrentIncarnation} = Membership)
  when Incarnation < CurrentIncarnation ->
    {[{membership, {alive, CurrentIncarnation, Membership#membership.local_member}}], Membership}.
