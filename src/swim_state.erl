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
-export([nack/1]).
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
          broadcasts             :: swim_broadcasts:broadcasts()
         }).

start_link(LocalMember, Broadcasts, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [LocalMember, Broadcasts, Opts], []).

join(Seeds) ->
    gen_server:cast(?MODULE, {join, Seeds}).

local_member() ->
    gen_server:call(?MODULE, local_member).

members() ->
    gen_server:call(?MODULE, members).

proxies(Target) ->
    gen_server:call(?MODULE, {proxies, Target}).

ack(Peer) ->
    gen_server:cast(?MODULE, {ack, Peer}).

nack(Peer) ->
    gen_server:cast(?MODULE, {nack, Peer}).

broadcasts(NumEvents) ->
    gen_server:call(?MODULE, {broadcasts, NumEvents}).

handle_event(Event) ->
    gen_server:cast(?MODULE, {broadcast_event, Event}).

%% @private
init([LocalMember, Broadcasts, Opts]) ->
    ProtocolPeriod = maps:get(protocol_period, Opts, 500),
    SuspicionFactor = maps:get(suspicion_factor, Opts, 3),
    Membership = swim_membership:new(LocalMember, ProtocolPeriod, SuspicionFactor),
    State =
        #state{
           membership      = Membership,
           ack_timeout     = maps:get(ack_timeout, Opts, 100),
           probe_timeout   = maps:get(probe_timeout, Opts, 350),
           protocol_period = ProtocolPeriod,
           num_proxies     = maps:get(num_proxies, Opts, 3),
           broadcasts      = Broadcasts
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
    Retransmit = swim_broadcasts:retransmit_limit(NumMembers, Broadcasts0),
    {Broadcast, Broadcasts} = swim_broadcasts:take(NumEvents, Retransmit, Broadcasts0),
    {reply, Broadcast, State#state{broadcasts = Broadcasts}};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

%% @private
handle_cast({ack, Member}, #state{current_probe = {Member, Incarnation}} = State0) ->
    #state{membership = Membership0, broadcasts = Broadcasts0} = State0,
    {Events, Membership} = swim_membership:alive(Member, Incarnation, Membership0),
    Broadcasts = swim_broadcasts:insert(Events, Broadcasts0),
    State = State0#state{
              membership = Membership,
              broadcasts = Broadcasts,
              current_probe = undefined},
    {noreply, State};
handle_cast({nack, Member}, #state{current_probe = {Member, _Incarnation}} = State) ->
    {noreply, State};
handle_cast({join, Seeds}, State) ->
    {noreply, handle_join(Seeds, State)};
handle_cast({broadcast_event, Event}, State) ->
    {Events, Membership} = swim_membership:handle_event(Event, State#state.membership),
    Broadcasts = swim_broadcasts:insert(Events, State#state.broadcasts),
    {noreply, State#state{membership = Membership, broadcasts = Broadcasts}};
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

handle_protocol_period(#state{current_probe = undefined} = State) ->
    send_next_probe(State);
handle_protocol_period(#state{current_probe = {Target, Incarnation}} = State0) ->
    #state{membership = Membership0, broadcasts = Broadcasts0} = State0,
    {Events, Membership} = swim_membership:suspect(Target, Incarnation, local, Membership0),
    Broadcasts = swim_broadcasts:insert(Events, Broadcasts0),
    State = State0#state{
              current_probe = undefined,
              membership = Membership,
              broadcasts = Broadcasts},
    send_next_probe(State).

send_next_probe(#state{probe_targets = []} = State) ->
    case probe_targets(State) of
        [] ->
            State;
        ProbeTargets ->
            send_next_probe(State#state{probe_targets = ProbeTargets})
    end;
send_next_probe(#state{probe_targets = [{Target, _Inc} = CurrentPing | ProbeTargets]} = State) ->
    #state{ack_timeout = AckTimeout, probe_timeout = ProbeTimeout} = State,
    ok = swim_failure:probe(Target, AckTimeout, ProbeTimeout),
    State#state{current_probe = CurrentPing, probe_targets = ProbeTargets}.

probe_targets(State) ->
    Members = swim_membership:members(State#state.membership),
    [{M, I} || {_, {M, _S, I}} <- lists:keysort(1, [{rand:uniform(), N} || N <- Members])].

schedule_next_protocol_period(State) ->
    swim_time:send_after(State#state.protocol_period, self(), protocol_period).

handle_join(Seeds, State) ->
    #state{membership = Membership0, broadcasts = Broadcasts0} = State,
    JoinEvent = {alive, 0, swim_membership:local_member(Membership0)},
    {Membership, Broadcasts} =
        lists:foldl(
          fun(Seed, {M0, B0}) ->
                  {Es, M} = swim_membership:alive(Seed, 0, M0),
                  B = swim_broadcasts:insert(Es, B0),
                  {M, B}
          end, {Membership0, swim_broadcasts:insert(JoinEvent, Broadcasts0)}, Seeds),
    State#state{membership = Membership, broadcasts = Broadcasts}.
