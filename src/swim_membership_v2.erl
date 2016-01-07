-module(swim_membership_v2).

-behavior(gen_server).

-include("swim.hrl").

-record(member_state, {
	  status        = alive :: member_status(),
	  incarnation   = 0     :: non_neg_integer(),
	  last_modified         :: integer()
	 }).

-type member_state() :: #member_state{}.

-record(state, {
	  me                       :: member(),
	  incarnation        = 0   :: non_neg_integer(),
	  members            = #{} :: maps:map(member(), member_state()),
	  suspicion_factor         :: pos_integer(),
	  protocol_period          :: pos_integer(),
	  membership_events        :: pid() | atom()
	 }).

-type state() :: #state{}.

-export([start_link/4, alive/3, suspect/3, faulty/3, members/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
	 terminate/2]).

-spec alive(pid(), member(), non_neg_integer()) -> list().
alive(Pid, Member, Incarnation) ->
    gen_server:call(Pid, {alive, Member, Incarnation}).

-spec suspect(pid(), member(), non_neg_integer()) -> list().
suspect(Pid, Member, Incarnation) ->
    gen_server:call(Pid, {suspect, Member, Incarnation}).

-spec faulty(pid(), member(), non_neg_integer()) -> list().
faulty(Pid, Member, Incarnation) ->
    gen_server:call(Pid, {faulty, Member, Incarnation}).

-spec members(pid()) -> [member()].
members(Pid) ->
    gen_server:call(Pid, members).

-spec start_link(member(), pos_integer(), pos_integer(), pid() | atom())
		-> {ok, pid()}.
start_link(Me, SuspicionFactor, ProtocolPeriod, MembershipEvents) ->
    gen_server:start_link(?MODULE,
			  [Me, SuspicionFactor, ProtocolPeriod, MembershipEvents],
			  []).

init([Me, SuspicionFactor, ProtocolPeriod, MembershipEvents]) ->
    {ok, #state{me=Me,
		suspicion_factor=SuspicionFactor,
		protocol_period=ProtocolPeriod,
		membership_events=MembershipEvents}}.

handle_call(members, _From, State) ->
    #state{members=Members} = State,
    {reply, maps:to_list(Members), State};
handle_call({alive, Member, Incarnation}, _From, #state{me=Member} = State) ->
    #state{incarnation=CurrentIncarnation} = State,
    case Incarnation < CurrentIncarnation of
	true ->
	    {reply, ok, State};
	false ->
	    {Events, NewState} = refute(Incarnation, State),
	    ok = publish_events(Events, NewState),
	    {reply, ok, NewState}
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
    ok = publish_events(Events, State),
    {reply, ok, State#state{members=NewMembers}};
handle_call({suspect, Member, Incarnation}, _From, #state{me=Member} = State) ->
    {Events, NewState} = refute(Incarnation, State),
    ok = publish_events(Events, NewState),
    {reply, ok, NewState};
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
    ok = publish_events(Events, State),
    {reply, ok, State#state{members=NewMembers}};
handle_call({faulty, Member, Incarnation}, _From, #state{me=Member} = State) ->
    {Events, NewState} = refute(Incarnation, State),
    ok = publish_events(Events, NewState),
    {reply, ok, NewState};
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
    ok = publish_events(Events, State),
    {reply, ok, State#state{members=NewMembers}};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({suspect_timeout, Member, SuspectedAt}, State) ->
    #state{members=Members} = State,
    {Events, NewMembers} = case maps:find(Member, Members) of
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
    ok = publish_events(Events, State),
    {noreply, State#state{members=NewMembers}};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

-spec suspicion_timeout(state()) -> pos_integer().
suspicion_timeout(State) ->
    #state{members=Members, suspicion_factor=Factor,
	   protocol_period=ProtocolPeriod} = State,
    round(math:log(maps:size(Members) + 1)) * Factor * ProtocolPeriod.

-spec suspicion_timer(member(), member_state(), state()) -> reference().
suspicion_timer(Member, MemberState, State) ->
    #member_state{last_modified=LastModified} = MemberState,
    Msg = {suspect_timeout, Member, LastModified},
    erlang:send_after(suspicion_timeout(State), self(), Msg).

-spec refute(non_neg_integer(), state()) -> {list(), state()}.
refute(Incarnation, #state{incarnation=CurrentIncarnation} = State)
  when Incarnation >= CurrentIncarnation ->
    #state{me=Me} = State,
    NewIncarnation = Incarnation + 1,
    {[{alive, Me, NewIncarnation}], State#state{incarnation=NewIncarnation}};
refute(Incarnation, #state{incarnation=CurrentIncarnation} = State)
  when Incarnation < CurrentIncarnation ->
    #state{me=Me} = State,
    {[{alive, Me, CurrentIncarnation}], State}.

publish_events(Events, State) ->
    #state{membership_events=MembershipEvents} = State,
    [ok = swim_events:membership(MembershipEvents, Event) || Event <- Events],
    ok.
