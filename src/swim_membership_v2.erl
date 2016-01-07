-module(swim_membership_v2).

-behavior(gen_server).

-include("swim.hrl").

-record(member_state, {
	  status        = alive :: member_status(),
	  incarnation   = 0     :: pos_integer(),
	  last_modified         :: integer()
	 }).

-type member_state() :: #member_state{}.

-record(state, {
	  me                       :: member(),
	  incarnation        = 0   :: pos_integer(),
	  members            = #{} :: maps:map(member(), member_state()),
	  suspicion_factor         :: pos_integer(),
	  protocol_period          :: pos_integer()
	 }).

-export([start_link/3, alive/3, suspect/3, faulty/3, members/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
	 terminate/2]).

alive(Pid, Member, Incarnation) ->
    gen_server:call(Pid, {alive, Member, Incarnation}).

suspect(Pid, Member, Incarnation) ->
    gen_server:call(Pid, {suspect, Member, Incarnation}).

faulty(Pid, Member, Incarnation) ->
    gen_server:call(Pid, {faulty, Member, Incarnation}).

members(Pid) ->
    gen_server:call(Pid, members).

start_link(Me, SuspicionFactor, ProtocolPeriod) ->
    gen_server:start_link(?MODULE, [Me, SuspicionFactor, ProtocolPeriod], []).

init([Me, SuspicionFactor, ProtocolPeriod]) ->
    {ok, #state{me=Me, suspicion_factor=SuspicionFactor,
		protocol_period=ProtocolPeriod}}.

handle_call(members, _From, State) ->
    #state{members=Members} = State,
    {reply, maps:to_list(Members), State};
handle_call({alive, Member, Incarnation}, _From, #state{me=Member} = State) ->
    #state{incarnation=CurrentIncarnation} = State,
    case Incarnation < CurrentIncarnation of
	true ->
	    {reply, [], State};
	false ->
	    {Events, NewState} = refute(Incarnation, State),
	    {reply, Events, NewState}
    end;
handle_call({alive, Member, Incarnation}, _From, State) ->
    #state{members=Members} = State,
    {Events, NewMembers} = case maps:find(Member, Members) of
			       {ok, MemberState} ->
				   #member_state{incarnation=CurrentIncarnation} = MemberState,
				   case Incarnation =< CurrentIncarnation of
				       true ->
					   {[], Members};
				       false ->
					   NewState = MemberState#member_state{status=alive,
									       incarnation=Incarnation,
									       last_modified=erlang:monotonic_time()},
					   Ms = maps:put(Member, NewState, Members),
					   {[{alive, Member, Incarnation}], Ms}
				   end;
			       error ->
				   NewState = #member_state{status=alive,
							    incarnation=Incarnation,
							    last_modified=erlang:monotonic_time()},
				   Ms = maps:put(Member, NewState, Members),
				   {[{alive, Member, Incarnation}], Ms}
			   end,
    {reply, Events, State#state{members=NewMembers}};
handle_call({suspect, Member, Incarnation}, _From, #state{me=Member} = State) ->
    {Events, NewState} = refute(Incarnation, State),
    {reply, Events, NewState};
handle_call({suspect, Member, Incarnation}, _From, State) ->
    #state{members=Members} = State,
    {Events, NewMembers} = case maps:find(Member, Members) of
			       {ok, MemberState} ->
				   #member_state{incarnation=CurrentIncarnation} = MemberState,
				   case Incarnation < CurrentIncarnation of
				       true ->
					   {[], State};
				       false ->
					   NewState = MemberState#member_state{status=suspect,
									       incarnation=Incarnation,
									       last_modified=erlang:monotonic_time()},
					   Ms = maps:put(Member, NewState, Members),
					   _ = suspicion_timer(Member, NewState, State),
					   {[{suspect, Member, Incarnation}], Ms}
				   end;
			       error ->
				   {[], Members}
			   end,
    {reply, Events, State#state{members=NewMembers}};
handle_call({faulty, Member, Incarnation}, _From, #state{me=Member} = State) ->
    {Events, NewState} = refute(Incarnation, State),
    {reply, Events, NewState};
handle_call({faulty, Member, Incarnation}, _From, State) ->
    #state{members=Members} = State,
    {Events, NewMembers} = case maps:find(Member, Members) of
			       {ok, MemberState} ->
				   #member_state{incarnation=CurrentIncarnation} = MemberState,
				   case Incarnation < CurrentIncarnation of
				       true ->
					   {[], State};
				       false ->
					   Ms = maps:remove(Member, Members),
					   {[{faulty, Member, CurrentIncarnation}], Ms}
				   end;
			       error ->
				   {[], Members}
			   end,
    {reply, Events, State#state{members=NewMembers}};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({suspect_timeout, Member, SuspectedAt}, State) ->
    #state{members=Members} = State,
    {_Events, NewMembers} = case maps:find(Member, Members) of
			       {ok, MemberState} ->
				   #member_state{last_modified=LastModified, incarnation=Incarnation} = MemberState,
				   case LastModified > SuspectedAt of
				       true ->
					   {[], Members};
				       false ->
					   Ms = maps:remove(Member, Members),
					   {[{faulty, Member, Incarnation}], Ms}
				   end;
			       error ->
				   {[], Members}
			   end,
    {noreply, State#state{members=NewMembers}};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

suspicion_timeout(State) ->
    #state{members=Members, suspicion_factor=Factor,
	   protocol_period=ProtocolPeriod} = State,
    round(math:log(maps:size(Members) + 1)) * Factor * ProtocolPeriod.

suspicion_timer(Member, MemberState, State) ->
    #member_state{last_modified=LastModified} = MemberState,
    Msg = {suspect_timeout, Member, LastModified},
    erlang:send_after(suspicion_timeout(State), self(), Msg).

refute(Incarnation, #state{incarnation=CurrentIncarnation} = State)
  when Incarnation >= CurrentIncarnation ->
    #state{me=Me} = State,
    NewIncarnation = Incarnation + 1,
    {[{alive, Me, NewIncarnation}], State#state{incarnation=NewIncarnation}};
refute(Incarnation, #state{incarnation=CurrentIncarnation} = State)
  when Incarnation < CurrentIncarnation ->
    #state{me=Me} = State,
    {[{alive, Me, CurrentIncarnation}], State}.
