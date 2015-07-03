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

-module(swim_membership_srv).
-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
	 terminate/2]).
-export([start_link/2, stop/0, members/0, is_member/1, status/1, size/0,
	 alive/1, suspect/1, leave/1, update/2, update/3, age_members/0,
	 members_with_status/0]).

start_link(Me, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Me, Opts], []).

stop() ->
    call(stop).

members() ->
    call(members).

members_with_status() ->
    call(members_with_status).

is_member(Member) ->
    call({is_member, Member}).

status(Member) ->
    call({status, Member}).

size() ->
    call(size).

alive(Member) ->
    call({alive, Member}).

suspect(Member) ->
    call({suspect, Member}).

leave(Member) ->
    call({leave, Member}).

update(Member, Status) ->
    call({update, Member, Status}).

update(Member, Status, Incarnation) ->
    call({update, Member, Status, Incarnation}).

age_members() ->
    call(age_members).

call(Msg) ->
    gen_server:call(?MODULE, Msg).

init([Me, Opts]) ->
    {ok, swim_membership:new(Me, Opts)}.

handle_call(members, _From, State) ->
    {reply, swim_membership:members(State), State};
handle_call(members_with_status, _From, State) ->
    {reply, swim_membership:members_with_status(State), State};
handle_call({is_member, Member}, _From, State) ->
    {reply, swim_membership:is_member(Member, State), State};
handle_call({status, Member}, _From, State) ->
    {reply, swim_membership:status(Member, State), State};
handle_call(size, _From, State) ->
    {reply, swim_membership:size(State), State};
handle_call({alive, Member}, _From, State) ->
    {Events, NewState} = swim_membership:alive(Member, State),
    {reply, Events, NewState};
handle_call({suspect, Member}, _From, State) ->
    {Events, NewState} = swim_membership:suspect(Member, State),
    {reply, Events, NewState};
handle_call({leave, Member}, _From, State) ->
    {Events, NewState} = swim_membership:leave(Member, State),
    {reply, Events, NewState};
handle_call({update, Member, Status}, _From, State) ->
    {Events, NewState} = swim_membership:update(Member, Status, State),
    {reply, Events, NewState};
handle_call({update, Member, Status, Incarnation}, _From, State) ->
    {Events, NewState} = swim_membership:update(Member, Status, Incarnation, State),
    {reply, Events, NewState};
handle_call(age_members, _From, State) ->
    {Events, NewState} = swim_membership:age_members(State),
    {reply, Events, NewState};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
