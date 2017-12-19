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

-include("swim.hrl").

-export([start_link/2]).
-export([local_member/0]).
-export([join/1]).
-export([members/0]).
-export([proxies/1]).
-export([ack/1]).
-export([nack/1]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-record(state, {
          protocol_period        :: pos_integer(),
          ack_timeout            :: pos_integer(),
          num_proxies            :: pos_integer(),
          current_probe          :: undefined | {member(), incarnation()},
          probe_targets    = []  :: [{member(), incarnation()}],
          membership             :: swim_membership:membership()
         }).

start_link(LocalMember, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [LocalMember, Opts], []).

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

%% @private
init([LocalMember, Opts]) ->
    ProtocolPeriod = maps:get(protocol_period, Opts, 500),
    SuspicionFactor = maps:get(suspicion_factor, Opts, 3),
    Membership = swim_membership:new(LocalMember, ProtocolPeriod, SuspicionFactor),
    State =
        #state{
           membership      = Membership,
           ack_timeout     = maps:get(ack_timeout, Opts, 50),
           protocol_period = ProtocolPeriod,
           num_proxies     = maps:get(num_proxies, Opts, 3)
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
handle_call(_Msg, _From, State) ->
    {noreply, State}.

%% @private
handle_cast({ack, Member}, #state{current_probe = {Member, Incarnation}} = State) ->
    {_Events, Membership} = swim_membership:alive(Member, Incarnation, State#state.membership),
    {noreply, State#state{membership = Membership, current_probe = undefined}};
handle_cast({nack, Member}, #state{current_probe = {Member, _Incarnation}} = State) ->
    {noreply, State};
handle_cast({join, Seeds}, State) ->
    NewMembership =
        lists:foldl(
          fun(Seed, Acc) ->
                  {_, M} = swim_membership:alive(Seed, 0, Acc),
                  M
          end, State#state.membership, Seeds),
    {noreply, State#state{membership = NewMembership}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(protocol_period, State) ->
    NewState = handle_protocol_period(State),
    schedule_next_protocol_period(NewState),
    {noreply, NewState};
handle_info({suspicion_timeout, Member, SuspectedAt}, State) ->
    {_Events, Membership} =
        swim_membership:suspicion_timeout(Member, SuspectedAt, State#state.membership),
    {noreply, State#state{membership = Membership}};
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
handle_protocol_period(#state{current_probe = {Target, Incarnation}} = State) ->
    {_Events, Membership} = swim_membership:suspect(Target, Incarnation, State#state.membership),
    send_next_probe(State#state{current_probe = undefined, membership = Membership}).

send_next_probe(#state{probe_targets = []} = State) ->
    case probe_targets(State) of
        [] ->
            State;
        ProbeTargets ->
            send_next_probe(State#state{probe_targets = ProbeTargets})
    end;
send_next_probe(#state{probe_targets = [{Target, _Inc} = CurrentPing | ProbeTargets]} = State) ->
    #state{ack_timeout = AckTimeout} = State,
    ok = swim_failure:probe(Target, AckTimeout),
    State#state{current_probe = CurrentPing, probe_targets = ProbeTargets}.

probe_targets(State) ->
    #state{membership = Membership} = State,
    Members = swim_membership:members(Membership),
    [{M, I} || {_, {M, _S, I}} <- lists:keysort(1, [{rand:uniform(), N} || N <- Members])].

schedule_next_protocol_period(State) ->
    swim_time:send_after(State#state.protocol_period, self(), protocol_period).
