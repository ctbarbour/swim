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

-export([start_link/4]).
-export([local_member/0]).
-export([local_state/0]).
-export([members/0]).
-export([proxies/1]).
-export([broadcasts/2]).
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
          membership             :: swim_membership:membership(),
          broadcasts             :: swim_broadcasts:broadcasts(),
          awareness              :: swim_awareness:awareness()
         }).

start_link(Membership, Broadcasts, Awareness, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Membership, Broadcasts, Awareness, Opts], []).

local_member() ->
    gen_server:call(?MODULE, local_member).

local_state() ->
    gen_server:call(?MODULE, local_state).

members() ->
    gen_server:call(?MODULE, members).

proxies(Target) ->
    gen_server:call(?MODULE, {proxies, Target}).

broadcasts(Fun, InitAcc) ->
    gen_server:call(?MODULE, {broadcasts, Fun, InitAcc}).

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
    {reply, swim_membership:members(State#state.membership), State};
handle_call(local_state, _From, State) ->
    {reply, swim_membership:local_state(State#state.membership), State};
handle_call({proxies, Target}, _From, State) ->
    Proxies = swim_membership:proxies(State#state.num_proxies, Target, State#state.membership),
    {reply, Proxies, State};
handle_call({broadcasts, Fun, InitAcc}, _From, State) ->
    #state{membership = Membership, broadcasts = Broadcasts0} = State,
    {Acc, Broadcasts1} = swim_broadcasts:takefold(Fun, InitAcc, Broadcasts0),
    NumMembers = swim_membership:size(Membership),
    Retransmits = swim_broadcasts:retransmit_limit(NumMembers, Broadcasts1),
    Broadcasts2 = swim_broadcasts:prune(Retransmits, Broadcasts1),
    {reply, Acc, State#state{broadcasts = Broadcasts2}};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

%% @private
handle_cast({ack, Member}, #state{current_probe = {Member, Incarnation}} = State) ->
    {noreply, handle_ack(Member, Incarnation, State)};
handle_cast({probe_timeout, Member, MissedNacks}, State) ->
    {noreply, handle_probe_timeout(Member, MissedNacks, State)};
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
        swim_membership:faulty(Member, SuspectedAt, local, State#state.membership),
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

handle_protocol_period(State) ->
    case swim_membership:probe_target(State#state.membership) of
        none ->
            State;
        {{Target, _} = Probe, Membership} ->
            ProbeTimeout = swim_awareness:scale(State#state.probe_timeout, State#state.awareness),
            ok = swim_failure:probe(Target, State#state.ack_timeout, ProbeTimeout),
            State#state{current_probe = Probe, membership = Membership}
    end.

schedule_next_protocol_period(State) ->
    #state{awareness = Awareness, protocol_period = ProtocolPeriod} = State,
    Timeout = swim_awareness:scale(ProtocolPeriod, Awareness),
    swim_time:send_after(Timeout, self(), protocol_period).
