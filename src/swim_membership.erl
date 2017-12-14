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

-behavior(gen_server).

-include("swim.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-define(LOCAL_MEMBER, {{127,0,0,1}, 5000}).
-endif.

-export([start_link/3, alive/3, suspect/3, faulty/3, members/1,
         local_member/1, set_status/3, num_members/1, opts/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).

-record(state, {
          local_member                :: member(),
          event_mgr_pid               :: pid() | module(),
          incarnation        = 0      :: non_neg_integer(),
          members            = #{}    :: maps:map(member(), member_state()),
          suspicion_factor   = 5      :: pos_integer(),
          protocol_period    = 1000   :: pos_integer()
         }).

-record(member_state, {
          status        = alive :: member_status(),
          incarnation   = 0     :: non_neg_integer(),
          last_modified         :: integer()
         }).

-type swim_membership_opt() :: {seeds, [member()]} |
                               {suspicion_factor, pos_integer()} |
                               {protocol_period, pos_integer()}.
-type member_state() :: #member_state{}.

-export_type([swim_membership_opt/0]).

-spec opts(list()) -> [swim_membership_opt()].
opts(Opts) ->
    opts(Opts, []).

-spec opts(list(), list()) -> [swim_membership_opt()].
opts([], Opts) ->
    Opts;
opts([{seeds, Seeds} | Rest], Opts) ->
    opts(Rest, [{seeds, Seeds} | Opts]);
opts([{suspicion_factor, Val} | Rest], Opts) ->
    opts(Rest, [{suspicion_factor, Val} | Opts]);
opts([{protocol_period, Val} | Rest], Opts) ->
    opts(Rest, [{protocol_period, Val} | Opts]);
opts([_ | Rest], Opts) ->
    opts(Rest, Opts).

%% @doc The number of known members in the gossip group, including the local member
-spec num_members(pid()) -> pos_integer().
num_members(Pid) ->
    gen_server:call(Pid, num_members).

%% @doc The identifier for the local member
-spec local_member(pid()) -> member().
local_member(Pid) ->
    gen_server:call(Pid, local).

%% @doc Forcibly set the status of a member
%%
%% In certain circumstances we want to be able to set the status of a member
%% without regard to the rules of the suspecicon mechanism which uses a member's
%% current status and incarnation.
%% @end
-spec set_status(pid(), member(), member_status()) -> ok.
set_status(Pid, Member, Status) ->
    gen_server:cast(Pid, {set_status, Member, Status}).

%% @doc Set the member status to alive
%%
%% If the member isn't known it's added to the membership and an event is
%% broadcast to the group. If the member is known and the incarnation is
%% greater than the current incarnation of the member, we update the incarnation
%% of member and broadcast an event to group. Otherwise, we do nothing.
%% @end
-spec alive(pid(), member(), incarnation())
           -> [{member_status(), member(), incarnation()}].
alive(Pid, Member, Incarnation) ->
    gen_server:call(Pid, {alive, Member, Incarnation}).

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
-spec suspect(pid(), member(), incarnation())
             -> [{member_status(), member(), incarnation()}].
suspect(Pid, Member, Incarnation) ->
    gen_server:call(Pid, {suspect, Member, Incarnation}).

%% @doc Remove the member from the group
%%
%% If the member isn't already known we do nothing. If the member is known
%% we remove the member and broadcast the change if the provided incarnation is
%% greater than the current incarnation of the member.
%% @end
-spec faulty(pid(), member(), incarnation())
            -> [{member_status(), member(), incarnation()}].
faulty(Pid, Member, Incarnation) ->
    gen_server:call(Pid, {faulty, Member, Incarnation}).

%% @doc A list of known members and their status
-spec members(pid()) -> [{member_status(), member(), incarnation()}].
members(Pid) ->
    gen_server:call(Pid, members).

-spec start_link(member(), pid(), [swim_membership_opt()]) -> {ok, pid()}.
start_link(LocalMember, EventMgrPid, Opts) ->
    gen_server:start_link(?MODULE, [LocalMember, EventMgrPid, Opts], []).

%% @private
handle_opts([], State) ->
    State;
handle_opts([{suspicion_factor, Val} | Rest], State) ->
    handle_opts(Rest, State#state{suspicion_factor=Val});
handle_opts([{protocol_period, Val} | Rest], State) ->
    handle_opts(Rest, State#state{protocol_period=Val});
handle_opts([{seeds, Seeds} | Rest], State) ->
    #state{members=Members} = State,
    NewMembers = lists:foldl(
                   fun(Member, Acc) ->
                           MemberState = #member_state{status=alive,
                                                       incarnation=0,
                                                       last_modified=erlang:monotonic_time()},
                           maps:put(Member, MemberState, Acc)
                   end, Members, Seeds),
    handle_opts(Rest, State#state{members=NewMembers});
handle_opts([_ | Rest], State) ->
    handle_opts(Rest, State).

%% @private
init([LocalMember, EventMgrPid, Opts]) ->
    State = handle_opts(Opts, #state{local_member=LocalMember,
                                     event_mgr_pid=EventMgrPid}),
    {ok, State}.

%% @private
handle_call(num_members, _From, State) ->
    #state{members=Members} = State,
    {reply, maps:size(Members) + 1, State};
handle_call(local, _From, State) ->
    {reply, State#state.local_member, State};
handle_call(members, _From, State) ->
    #state{members=Members} = State,
    M = maps:fold(
          fun(Member, MemberState, Acc) ->
                  #member_state{status=Status, incarnation=Inc} = MemberState,
                  [{Member, Status, Inc} | Acc]
          end, [], Members),
    {reply, M, State};
handle_call({alive, Member, Incarnation}, _From, #state{local_member=Member} = State) ->
    #state{incarnation=CurrentIncarnation} = State,
    case Incarnation =< CurrentIncarnation of
        true ->
            {reply, ok, State};
        false ->
            {Events, NewState} = refute(Incarnation, State),
            ok = publish_events(Events, State),
            {reply, Events, NewState}
    end;
handle_call({alive, Member, Incarnation}, _From, State) ->
    #state{members=Members} = State,
    {Events, NewMembers} =
        case maps:find(Member, Members) of
            {ok, MemberState} ->
                #member_state{incarnation=CurrentIncarnation} = MemberState,
                case Incarnation > CurrentIncarnation of
                    true ->
                        NewState = MemberState#member_state{status=alive,
                                                            incarnation=Incarnation,
                                                            last_modified=erlang:monotonic_time()},
                        Ms = maps:put(Member, NewState, Members),
                        {[{alive, Member, Incarnation}], Ms};
                    false ->
                        {[], Members}
                end;
            error ->
                NewState = #member_state{status=alive,
                                         incarnation=Incarnation,
                                         last_modified=erlang:monotonic_time()},
                Ms = maps:put(Member, NewState, Members),
                {[{alive, Member, Incarnation}], Ms}
        end,
    ok = publish_events(Events, State),
    {reply, Events, State#state{members=NewMembers}};
handle_call({suspect, Member, Incarnation}, _From, #state{local_member=Member} = State) ->
    {Events, NewState} = refute(Incarnation, State),
    {reply, Events, NewState};
handle_call({suspect, Member, Incarnation}, _From, State) ->
    #state{members=Members} = State,
    {Events, NewMembers} =
        case maps:find(Member, Members) of
            {ok, MemberState} ->
                case MemberState of
                    #member_state{status=suspect, incarnation=CurrentIncarnation}
                      when Incarnation > CurrentIncarnation ->
                        NewState = MemberState#member_state{status=suspect,
                                                            incarnation=Incarnation,
                                                            last_modified=erlang:monotonic_time()},
                        Ms = maps:put(Member, NewState, Members),
                        _ = suspicion_timer(Member, NewState, State),
                        {[{suspect, Member, Incarnation}], Ms};
                    #member_state{status=alive, incarnation=CurrentIncarnation}
                      when Incarnation >= CurrentIncarnation ->
                        NewState = MemberState#member_state{status=suspect,
                                                            incarnation=Incarnation,
                                                            last_modified=erlang:monotonic_time()},
                        Ms = maps:put(Member, NewState, Members),
                        _ = suspicion_timer(Member, NewState, State),
                        {[{suspect, Member, Incarnation}], Ms};
                    _ ->
                        {[], Members}
                end;
            error ->
                {[], Members}
        end,
    {reply, Events, State#state{members=NewMembers}};
handle_call({faulty, Member, Incarnation}, _From, #state{local_member=Member} = State) ->
    {Events, NewState} = refute(Incarnation, State),
    {reply, Events, NewState};
handle_call({faulty, Member, Incarnation}, _From, State) ->
    #state{members=Members} = State,
    {Events, NewMembers} =
        case maps:find(Member, Members) of
            {ok, MemberState} ->
                #member_state{incarnation=CurrentIncarnation} = MemberState,
                case Incarnation < CurrentIncarnation of
                    true ->
                        {[], Members};
                    false ->
                        Ms = maps:remove(Member, Members),
                        {[{faulty, Member, CurrentIncarnation}], Ms}
                end;
            error ->
                {[], Members}
        end,
    ok = publish_events(Events, State),
    {reply, Events, State#state{members=NewMembers}};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

%% @private
handle_cast({set_status, Member, Status}, State) ->
    #state{members=Members} = State,
    {_Events, NewMembers} =
        case maps:find(Member, Members) of
            {ok, MemberState} ->
                NewMemberState = MemberState#member_state{status=Status,
                                                          last_modified=erlang:monotonic_time()},
                Ms = maps:put(Member, NewMemberState, Members),
                case Status of
                    suspect ->
                        _ = suspicion_timer(Member, NewMemberState, State),
                        {[{suspect, Member, NewMemberState#member_state.incarnation}], Ms};
                    _ ->
                        {[], Ms}
                end;
            error ->
                {[], Members}
        end,
    {noreply, State#state{members=NewMembers}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({suspect_timeout, Member, SuspectedAt}, State) ->
    #state{members=Members} = State,
    {Events, NewMembers} =
        case maps:find(Member, Members) of
            {ok, MemberState} ->
                case MemberState of
                    #member_state{incarnation=SuspectedAt, status=suspect} ->
                        Ms = maps:remove(Member, Members),
                        {[{faulty, Member, SuspectedAt}], Ms};
                    _ ->
                        {[], Members}
                end;
            error ->
                {[], Members}
        end,
    ok = publish_events(Events, State),
    {noreply, State#state{members=NewMembers}};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
suspicion_timeout(State) ->
    #state{members=Members, suspicion_factor=Factor,
           protocol_period=ProtocolPeriod} = State,
    round(math:log(maps:size(Members) + 2)) * Factor * ProtocolPeriod.

%% @private
suspicion_timer(Member, MemberState, State) ->
    #member_state{incarnation=Incarnation} = MemberState,
    Msg = {suspect_timeout, Member, Incarnation},
    erlang:send_after(suspicion_timeout(State), self(), Msg).

%% @private
refute(Incarnation, #state{incarnation=CurrentIncarnation} = State)
  when Incarnation >= CurrentIncarnation ->
    #state{local_member=LocalMember} = State,
    NewIncarnation = Incarnation + 1,
    {[{alive, LocalMember, NewIncarnation}], State#state{incarnation=NewIncarnation}};
refute(Incarnation, #state{incarnation=CurrentIncarnation} = State)
  when Incarnation < CurrentIncarnation ->
    #state{local_member=LocalMember} = State,
    {[{alive, LocalMember, CurrentIncarnation}], State}.

%% @private
publish_events(Events, State) ->
    #state{event_mgr_pid=EventMgrPid} = State,
    _ = [swim_broadcasts:membership(EventMgrPid, Event) || Event <- Events],
    ok.

-ifdef(TEST).

membership_v2_local_member_test() ->
    {ok, EventMgrPid} = gen_event:start_link(),
    {ok, Membership} = swim_membership:start_link(?LOCAL_MEMBER, EventMgrPid, []),
    ?assertMatch(?LOCAL_MEMBER, swim_membership:local_member(Membership)).

membership_v2_suspect_timeout_test() ->
    {ok, EventMgrPid} = gen_event:start_link(),
    Seed = {{10,10,10,10}, 5000},
    {ok, Membership} = swim_membership:start_link(?LOCAL_MEMBER, EventMgrPid,
                                                  [{suspicion_factor, 1},
                                                   {protocol_period, 100},
                                                   {seeds, [Seed]}]),
    _ = swim_membership:suspect(Membership, Seed, 1),
    ok = timer:sleep(100),
    Members = [M || {M, _, _} <- swim_membership:members(Membership)],
    ok = gen_event:stop(EventMgrPid),
    ?assertNot(lists:member(Seed, Members)).

membership_v2_suspect_timeout_refute_test() ->
    {ok, EventMgrPid} = gen_event:start_link(),
    Seed = {{10,10,10,10}, 5000},
    {ok, Membership} = swim_membership:start_link(?LOCAL_MEMBER, EventMgrPid,
                                                  [{suspicion_factor, 10},
                                                   {protocol_period, 5000},
                                                   {seeds, [Seed]}]),
    _ = swim_membership:suspect(Membership, Seed, 1),
    _ = swim_membership:alive(Membership, Seed, 2),
    Members = swim_membership:members(Membership),
    ok = gen_event:stop(EventMgrPid),
    ?assert(lists:member({Seed, alive, 2}, Members)).

-endif.
