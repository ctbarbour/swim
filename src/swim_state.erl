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

-module(swim_state).
-behavior(gen_server).

-export([start_link/3]).
-export([local_member/0]).
-export([join/1]).
-export([members/0]).
-export([proxies/1]).
-export([broadcasts/1]).
-export([ack/1]).
-export([probe_timeout/2]).
-export([handle_event/1]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-record(state, {
          protocol_period        :: pos_integer(),
          ack_timeout            :: pos_integer(),
          probe_timeout          :: pos_integer(),
          num_proxies            :: pos_integer(),
          current_probe          :: undefined | {swim:member(), swim:incarnation()},
          probe_targets    = []  :: [{swim:member(), swim:incarnation()}],
          membership             :: swim_membership:membership(),
          broadcasts             :: swim_broadcasts:broadcasts(),
          awareness              :: swim_awareness:awareness()
         }).

start_link(Membership, Broadcasts, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Membership, Broadcasts, Opts], []).

local_member() ->
    gen_server:call(?MODULE, local_member).

members() ->
    gen_server:call(?MODULE, members).

proxies(Target) ->
    gen_server:call(?MODULE, {proxies, Target}).

broadcasts(NumEvents) ->
    gen_server:call(?MODULE, {broadcasts, NumEvents}).

join(Seeds) ->
    gen_server:cast(?MODULE, {join, Seeds}).

ack(Member) ->
    gen_server:cast(?MODULE, {ack, Member}).

probe_timeout(Member, MissedNacks) ->
    gen_server:cast(?MODULE, {probe_timeout, Member, MissedNacks}).

handle_event(Event) ->
    gen_server:cast(?MODULE, {broadcast_event, Event}).

%% @private
init([Membership, Broadcasts, Awareness, Opts]) ->
    State =
        #state{
           membership      = Membership,
           broadcasts      = Broadcasts,
           awareness       = Awareness,
           ack_timeout     = maps:get(ack_timeout, Opts),
           probe_timeout   = maps:get(probe_timeout, Opts),
           protocol_period = maps:get(protocol_period, Opts),
           num_proxies     = maps:get(num_proxies, Opts)
          },
    self() ! protocol_period,
    {ok, State}.

%% @private
handle_call(local_member, _From, State) ->
    #state{membership = Membership} = State,
    {reply, swim_membership:local_member(Membership), State};
handle_call(members, _From, State) ->
    #state{membership = Membership} = State,
    {reply, swim_membership:members(Membership), State};
handle_call({proxies, Target}, _From, State) ->
    Proxies = lists:sublist(
                [M || {M, _I} <- probe_targets(State), M =/= Target],
                State#state.num_proxies),
    {reply, Proxies, State};
handle_call({broadcasts, NumEvents}, _From, State) ->
    #state{membership = Membership, broadcasts = Broadcasts0} = State,
    NumMembers = swim_membership:size(Membership),
    {ToSend, Broadcasts1} = swim_broadcasts:take(NumEvents, Broadcasts0),
    Retransmits = swim_broadcasts:retransmit_limit(NumMembers, Broadcasts1),
    Broadcasts2 = swim_broadcasts:prune(Retransmits, Broadcasts1),
    {reply, ToSend, State#state{broadcasts = Broadcasts2}};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

%% @private
handle_cast({ack, Member}, #state{current_probe = {Member, Incarnation}} = State) ->
    {noreply, handle_ack(Member, Incarnation, State)};
handle_cast({probe_timeout, Member, MissedNacks}, State) ->
    {noreply, handle_probe_timeout(Member, MissedNacks, State)};
handle_cast({join, Seeds}, State) ->
    {noreply, handle_join(Seeds, State)};
handle_cast({broadcast_event, Event}, State) ->
    {Events, Membership} = swim_membership:handle_event(Event, State#state.membership),
    Awareness =
        case swim_membership:refuted(Events, Membership) of
            true -> swim_awareness:failure(State#state.awareness);
            false -> State#state.awareness
        end,
    Broadcasts = swim_broadcasts:insert(Events, State#state.broadcasts),
    {noreply, State#state{membership = Membership, broadcasts = Broadcasts, awareness = Awareness}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(protocol_period, State) ->
    NewState = handle_protocol_period(State),
    schedule_next_protocol_period(NewState),
    {noreply, NewState};
handle_info({suspicion_timeout, Member, SuspectedAt}, State) ->
    {Events, Membership} =
        swim_membership:suspicion_timeout(Member, SuspectedAt, State#state.membership),
    Broadcasts = swim_broadcasts:insert(Events, State#state.broadcasts),
    {noreply, State#state{membership = Membership, broadcasts = Broadcasts}};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

handle_probe_timeout(Member, MissedNacks, #state{current_probe = {Member, Incarnation}} = State) ->
    #state{membership = Membership0, broadcasts = Broadcasts0, awareness = Awareness0} = State,
    {Events, Membership} = swim_membership:suspect(Member, Incarnation, local, Membership0),
    Broadcasts = swim_broadcasts:insert(Events, Broadcasts0),
    Awareness = swim_awareness:failure(MissedNacks + 1, Awareness0),
    State#state{
      current_probe = undefined,
      membership    = Membership,
      broadcasts    = Broadcasts,
      awareness     = Awareness
     };
handle_probe_timeout(_Member, _MissedNacks, State) ->
    State.

handle_ack(Member, Incarnation, State) ->
    #state{membership = Membership0, broadcasts = Broadcasts0, awareness = Awareness0} = State,
    {Events, Membership} = swim_membership:alive(Member, Incarnation, Membership0),
    Broadcasts = swim_broadcasts:insert(Events, Broadcasts0),
    Awareness = swim_awareness:success(Awareness0),
    State#state{
      membership    = Membership,
      broadcasts    = Broadcasts,
      awareness     = Awareness,
      current_probe = undefined
     }.

handle_protocol_period(#state{probe_targets = []} = State) ->
    case probe_targets(State) of
        [] ->
            State;
        ProbeTargets ->
            handle_protocol_period(State#state{probe_targets = ProbeTargets})
    end;
handle_protocol_period(#state{probe_targets = [{Target, _Inc} = Probe | ProbeTargets]} = State) ->
    ProbeTimeout = swim_awareness:scale(State#state.probe_timeout, State#state.awareness),
    ok = swim_failure:probe(Target, State#state.ack_timeout, ProbeTimeout),
    State#state{current_probe = Probe, probe_targets = ProbeTargets}.

probe_targets(State) ->
    Members = swim_membership:members(State#state.membership),
    [{M, I} || {_, {M, _S, I}} <- lists:keysort(1, [{rand:uniform(), N} || N <- Members])].

schedule_next_protocol_period(State) ->
    #state{awareness = Awareness, protocol_period = ProtocolPeriod} = State,
    Timeout = swim_awareness:scale(ProtocolPeriod, Awareness),
    swim_time:send_after(Timeout, self(), protocol_period).

handle_join(Seeds, State) ->
    #state{membership = Membership0, broadcasts = Broadcasts0} = State,
    JoinEvent = {membership, {alive, 0, swim_membership:local_member(Membership0)}},
    {Membership, Broadcasts} =
        lists:foldl(
          fun(Seed, {M0, B0}) ->
                  {Es, M} = swim_membership:alive(Seed, 0, M0),
                  {M, swim_broadcasts:insert(Es, B0)}
          end, {Membership0, swim_broadcasts:insert(JoinEvent, Broadcasts0)}, Seeds),
    State#state{membership = Membership, broadcasts = Broadcasts}.
