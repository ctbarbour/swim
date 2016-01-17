-module(swim_membership_v2).

-behavior(gen_server).

-include("swim.hrl").


-record(state, {
	  me                          :: member(),
	  incarnation        = 0      :: non_neg_integer(),
	  members            = #{}    :: maps:map(member(), member_state()),
	  suspicion_factor   = 5      :: pos_integer(),
	  protocol_period    = 1000   :: pos_integer(),
	  membership_events  = []     :: [{member(), non_neg_integer(), binary()}],
	  retransmit_factor  = 5      :: pos_integer()
	 }).

-record(member_state, {
	  status        = alive :: member_status(),
	  incarnation   = 0     :: non_neg_integer(),
	  last_modified         :: integer()
	 }).

-record(progress, {
	  size_remaining :: pos_integer(),
	  broadcasts = [] :: [binary()],
	  max_transmissions :: non_neg_integer()
	 }).

-type member_state() :: #member_state{}.
-type state() :: #state{}.

-type membership_opt() :: {suspicion_factor, pos_integer()} |
			  {protocol_period, pos_integer()} |
			  {retransmit_factor, pos_integer()} |
			  {seeds, [member()]}.

-export([start_link/2, alive/3, suspect/3, faulty/3, members/1,
	 events/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
	 terminate/2]).

events(Pid, MaxSize) ->
    gen_server:call(Pid, {events, MaxSize}).

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

-spec start_link(member(), [membership_opt()]) -> {ok, pid()}.
start_link(Me, Opts) ->
    gen_server:start_link(?MODULE, [Me, Opts], []).

handle_opts([], State) ->
    State;
handle_opts([{suspicion_factor, Val} | Rest], State) ->
    handle_opts(Rest, State#state{suspicion_factor=Val});
handle_opts([{protocol_period, Val} | Rest], State) ->
    handle_opts(Rest, State#state{protocol_period=Val});
handle_opts([{retransmit_factor, Val} | Rest], State) ->
    handle_opts(Rest, State#state{retransmit_factor=Val});
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

init([Me, Opts]) ->
    State = handle_opts(Opts, #state{me=Me}),
    {ok, State}.

handle_call({events, MaxSize}, _From, State) ->
    #state{retransmit_factor=RetransmitFactor,
	   membership_events=Events,
	   members=Members} = State,
    NumMembers = maps:size(Members),
    MaxTransmissions = round(RetransmitFactor * math:log(NumMembers + 1)),
    Progress = #progress{max_transmissions=MaxTransmissions,
			 size_remaining=MaxSize},
    {Broadcasts, Remaining} = dequeue(Progress, Events),
    {reply, Broadcasts, State#state{membership_events=Remaining}};
handle_call(members, _From, State) ->
    #state{members=Members} = State,
    M = maps:fold(
	  fun(Member, MemberState, Acc) ->
		  #member_state{status=Status, incarnation=Inc} = MemberState,
		  [{Member, Status, Inc} | Acc]
	  end, [], Members),
    {reply, M, State};
handle_call({alive, Member, Incarnation}, _From, #state{me=Member} = State) ->
    #state{incarnation=CurrentIncarnation} = State,
    case Incarnation < CurrentIncarnation of
	true ->
	    {reply, ok, State};
	false ->
	    {Events, NewState} = refute(Incarnation, State),
	    {reply, ok, enqueue(Events, NewState)}
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
    {reply, ok, enqueue(Events, State#state{members=NewMembers})};
handle_call({suspect, Member, Incarnation}, _From, #state{me=Member} = State) ->
    {Events, NewState} = refute(Incarnation, State),
    {reply, ok, enqueue(Events, NewState)};
handle_call({suspect, Member, Incarnation}, _From, State) ->
    #state{members=Members} = State,
    {Events, NewMembers} =
	case maps:find(Member, Members) of
	    {ok, MemberState} ->
		#member_state{incarnation=CurrentIncarnation} = MemberState,
		case Incarnation < CurrentIncarnation of
		    true ->
			{[], Members};
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
    {reply, ok, enqueue(Events, State#state{members=NewMembers})};
handle_call({faulty, Member, Incarnation}, _From, #state{me=Member} = State) ->
    {Events, NewState} = refute(Incarnation, State),
    {reply, ok, enqueue(Events, NewState)};
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
    NewState = enqueue(Events, State#state{members=NewMembers}),
    {reply, ok, NewState};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({suspect_timeout, Member, SuspectedAt}, State) ->
    #state{members=Members} = State,
    {Events, NewMembers} =
	case maps:find(Member, Members) of
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
    NewState = enqueue(Events, State#state{members=NewMembers}),
    {noreply, NewState};
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

dequeue(Progress, Events) ->
    dequeue(Progress, lists:keysort(2, Events), []).

dequeue(Progress, [], Remaining) ->
    #progress{broadcasts=Broadcasts} = Progress,
    {Broadcasts, Remaining};
dequeue(Progress, [NextBroadcast | Rest] = L, Remaining) ->
    #progress{size_remaining=SizeRemaining, broadcasts=Broadcasts,
	      max_transmissions=MaxTransmissions} = Progress,
    {Member, Transmissions, Msg} = NextBroadcast,
    case SizeRemaining - 15 of
	NewSize when NewSize > 0 ->
	    case Transmissions + 1 of
		T when T >= MaxTransmissions ->
		    dequeue(Progress#progress{size_remaining=NewSize,
					      broadcasts=[Msg | Broadcasts]},
			    Rest, Remaining);
		T ->
		    dequeue(Progress#progress{size_remaining=NewSize,
					      broadcasts=[Msg | Broadcasts]},
			    Rest, [{Member, T, Msg} | Remaining])
	    end;
	NewSize when NewSize =< 0 ->
	    {Broadcasts, lists:flatten([L | Remaining])}
    end.

enqueue([], State) ->
    State;
enqueue([{_Type, Subject, _Inc} = Event | Rest], State) ->
    #state{membership_events=MembershipEvents} = State,
    FilteredEvents = lists:filter(fun({S, _, _}) ->
					  S =/= Subject
				  end, MembershipEvents),
    NewMembershipEvents = [{Subject, 0, Event} | FilteredEvents],
    enqueue(Rest, State#state{membership_events=NewMembershipEvents}).
