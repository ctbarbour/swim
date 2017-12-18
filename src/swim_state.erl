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
-export([leave/0]).
-export([members/0]).
-export([proxies/1]).
-export([ack/2]).
-export([nack/2]).

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
          current_ping           :: undefined | {member(), incarnation()},
          ping_targets    = []   :: [{member(), incarnation()}],
          sequence        = 0    :: non_neg_integer(),
          membership             :: swim_membership:membership()
         }).

start_link(LocalMember, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [LocalMember, Opts], []).

join(Seeds) ->
    gen_server:cast(?MODULE, {join, Seeds}).

leave() ->
    gen_server:cast(?MODULE, leave).

local_member() ->
    gen_server:call(?MODULE, local_member).

members() ->
    gen_server:call(?MODULE, members).

proxies(Target) ->
    gen_server:call(?MODULE, {proxies, Target}).

ack(Peer, Sequence) ->
    gen_server:cast(?MODULE, {ack, Peer, Sequence}).

nack(Peer, Sequence) ->
    gen_server:cast(?MODULE, {nack, Peer, Sequence}).

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
           num_proxies     = maps:get(num_proxies, Opts, 3),
           sequence        = maps:get(sequence, Opts, 0)
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
                [M || {M, _I} <- ping_targets(State), M =/= Target],
                State#state.num_proxies),
    {reply, Proxies, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

%% @private
handle_cast({ack, Peer, Incarnation}, #state{current_ping = {Peer, _Inc}} = State) ->
    {_Events, Membership} = swim_membership:alive(Peer, Incarnation, State#state.membership),
    {noreply, State#state{membership = Membership, current_ping = undefined}};
handle_cast({ping, Peer}, State) ->
    {_Events, Membership} = swim_membership:alive(Peer, 0, State#state.membership),
    {noreply, State#state{membership = Membership}};
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
handle_info(protocol_period, #state{sequence = Sequence} = State) ->
    NewState = handle_protocol_period(State),
    _TRef = schedule_next_protocol_period(NewState),
    {noreply, NewState#state{sequence = Sequence + 1}};
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

handle_protocol_period(#state{current_ping = undefined} = State) ->
    send_next_ping(State);
handle_protocol_period(#state{current_ping = {Target, Incarnation}} = State) ->
    {_Events, Membership} = swim_membership:suspect(Target, Incarnation, State#state.membership),
    send_next_ping(State#state{current_ping = undefined, membership = Membership}).

send_next_ping(#state{ping_targets = []} = State) ->
    case ping_targets(State) of
        [] ->
            State;
        PingTargets ->
            send_next_ping(State#state{ping_targets = PingTargets})
    end;
send_next_ping(#state{ping_targets = [{Target, _Inc} = CurrentPing | PingTargets]} = State) ->
    #state{sequence = Sequence, ack_timeout = AckTimeout} = State,
    ok = swim_failure:probe(Target, Sequence, AckTimeout),
    State#state{current_ping = CurrentPing, ping_targets = PingTargets}.

ping_targets(State) ->
    #state{membership = Membership} = State,
    Members = swim_membership:members(Membership),
    [{M, I} || {_, {M, _S, I}} <- lists:keysort(1, [{rand:uniform(), N} || N <- Members])].

schedule_next_protocol_period(State) ->
    #state{protocol_period = Timeout} = State,
    swim_time:send_after(Timeout, self(), protocol_period).
