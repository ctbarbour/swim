-module(swim).
-behavior(gen_server).

-include("swim.hrl").

-export([start_link/3, start_link/4, members/1, publish/2, local_member/1,
	 stop/1, rotate_keys/2, child_spec/4, subscribe/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
	 terminate/2]).

-record(state, {
	  owner                  :: pid(),
	  protocol_period = 3000 :: pos_integer(),
	  ack_proxies     = 3    :: pos_integer(),
	  ack_timeout     = 750  :: pos_integer(),
	  current_ping           :: ping(),
	  local_member           :: member(),
	  proxy_pings     = []   :: [ping()],
	  ping_targets    = []   :: [{member(), incarnation()}],
	  sequence        = 0    :: non_neg_integer(),
	  membership             :: pid(),
	  transport              :: pid(),
	  broadcasts             :: pid()
	 }).

-record(ping, {
	  sequence :: non_neg_integer(),
	  origin :: member(),
	  terminal :: member(),
	  incarnation :: incarnation(),
	  ref :: reference(),
	  tref :: reference(),
	  sent :: pos_integer()
	 }).

-type ping() :: #ping{}.

start_link(Name, LocalMember, Keys, Opts) ->
    gen_server:start_link({local, Name}, ?MODULE, [self(), LocalMember, Keys, Opts], []).

start_link(LocalMember, Keys, Opts) ->
    gen_server:start_link(?MODULE, [self(), LocalMember, Keys, Opts], []).

rotate_keys(Pid, NewKey) ->
    gen_server:call(Pid, {rotate_keys, NewKey}).

local_member(Pid) ->
    gen_server:call(Pid, local_member).

members(Pid) ->
    gen_server:call(Pid, members).

publish(Pid, Event) ->
    gen_server:cast(Pid, {publish, Event}).

subscribe(Pid) ->
    gen_server:cast(Pid, {subscribe, self()}).

child_spec(Name, LocalMember, Keys, Opts) ->
    {Name, {swim, start_link, [Name, LocalMember, Keys, Opts]},
     permanent, 5000, worker, [swim]}.

stop(Pid) ->
    gen_server:call(Pid, stop).

start_transport({Ip, Port}, Keys) ->
    swim_transport:start_link(Ip, Port, Keys).

start_event_handlers(Owner) ->
    {ok, SwimEvents} = gen_event:start_link(),
    ok = gen_event:add_sup_handler(SwimEvents, swim_subscriptions, [Owner]),
    ok = gen_event:add_sup_handler(SwimEvents, swim_broadcasts, []),
    {ok, SwimEvents}.

init([Owner, LocalMember, Keys, Opts]) ->
    {ok, SwimEvents} = start_event_handlers(Owner),
    {ok, Membership} = swim_membership:start_link(LocalMember, SwimEvents, Opts),
    {ok, Transport} = start_transport(LocalMember, Keys),
    State = init(Opts, #state{local_member=LocalMember, owner=Owner,
			      broadcasts=SwimEvents,
			      membership=Membership, transport=Transport}),
    self() ! protocol_period,
    {ok, State}.

init([], State) ->
    State;
init([{protocol_period, Val} | Rest], State)
  when is_integer(Val) andalso Val > 0 ->
    init(Rest, State#state{protocol_period=Val});
init([{ack_proxies, Val} | Rest], State)
  when is_integer(Val) andalso Val > 0 ->
    init(Rest, State#state{ack_proxies=Val});
init([{ack_timeout, Val} | Rest], State)
  when is_integer(Val) andalso Val > 0 ->
    init(Rest, State#state{ack_timeout=Val});
init([_ | Rest], State) ->
    init(Rest, State).

handle_call({rotate_keys, NewKey}, _From, State) ->
    #state{transport=Transport} = State,
    ok = swim_transport:rotate_keys(Transport, NewKey),
    {reply, ok, State};
handle_call(local_member, _From, State) ->
    #state{local_member=LocalMember} = State,
    {reply, LocalMember, State};
handle_call(stop, _From, State) ->
    #state{transport=Transport} = State,
    ok = swim_transport:close(Transport),
    {stop, normal, ok, State};
handle_call(members, _From, State) ->
    #state{membership=Membership} = State,
    {reply, swim_membership:members(Membership), State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({subscribe, Subscriber}, State) ->
    #state{broadcasts=EventMgrPid} = State,
    ok = swim_subscriptions:subscribe(EventMgrPid, user, Subscriber),
    ok = swim_subscriptions:subscribe(EventMgrPid, membership, Subscriber),
    {noreply, State};
handle_cast({publish, Event}, State) ->
    #state{broadcasts=EventMgrPid} = State,
    ok = swim_broadcasts:user(EventMgrPid, Event),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(protocol_period, State) ->
    NewState = handle_protocol_period(State),
    ok = schedule_next_protocol_period(NewState),
    {noreply, NewState};
handle_info({ack_timeout, Ref}, State) ->
    NewState = handle_ack_timeout(Ref, State),
    {noreply, NewState};
handle_info({{ack, Sequence, Responder, Events}, _From}, State) ->
    #state{current_ping=CurrentPing} = State,
    NewState = handle_ack(Sequence, Responder, CurrentPing, State),
    _ = handle_events(Events, State),
    {noreply, NewState};
handle_info({{ping, Sequence, Events}, From}, State) ->
    NewState = handle_ping(Sequence, From, State),
    _ = handle_events(Events, State),
    {noreply, NewState};
handle_info({{ping_req, Sequence, Terminal}, Origin}, State) ->
    NewState = handle_ping_req(Sequence, Terminal, Origin, State),
    {noreply, NewState};
handle_info(Info, State) ->
    ok = error_logger:info_msg("Received unmatched info: ~p", [Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

handle_protocol_period(#state{current_ping=undefined} = State) ->
    send_next_ping(State);
handle_protocol_period(#state{current_ping=Ping} = State) ->
    #state{membership=Membership} = State,
    #ping{terminal=Terminal} = Ping,
    _ = swim_membership:set_status(Membership, Terminal, suspect),
    send_next_ping(State#state{current_ping=undefined}).

send_next_ping(#state{ping_targets=[]} = State) ->
    case ping_targets(State) of
	[] ->
	    State;
	PingTargets ->
	    send_next_ping(State#state{ping_targets=PingTargets})
    end;
send_next_ping(#state{ping_targets=[PingTarget|PingTargets]} = State) ->
    #state{local_member=LocalMember, sequence=Sequence} = State,
    {ok, Ping} = send_ping(PingTarget, LocalMember, Sequence, State),
    State#state{current_ping=Ping, ping_targets=PingTargets}.

ping_targets(State) ->
    #state{membership=Membership} = State,
    Members = swim_membership:members(Membership),
    [{M, I} || {_, {M, _S, I}} <- lists:keysort(1, [{random:uniform(), N} || N <- Members])].

create_ack_timer(Ref, State) ->
    #state{ack_timeout=Timeout} = State,
    erlang:send_after(Timeout, self(), {ack_timeout, Ref}).

schedule_next_protocol_period(State) ->
    #state{protocol_period=Timeout} = State,
    _TRef = erlang:send_after(Timeout, self(), protocol_period),
    ok.

handle_ack_timeout(_Ref, #state{current_ping=undefined} = State) ->
    State;
handle_ack_timeout(Ref, #state{current_ping=#ping{ref=Ref} = Ping} = State) ->
    #state{ack_proxies=AckProxies, sequence=Sequence} = State,
    #ping{terminal=Terminal} = Ping,
    _ = [PingReq || Proxy <- proxies(AckProxies, State),
		    PingReq <- [send_ping_req(Proxy, Terminal, Sequence, State)]],
    State.

proxies(AckProxies, State) ->
    #state{current_ping=#ping{terminal=Terminal}} = State,
    lists:sublist([M || {M, _I} <- ping_targets(State),
			 M /= Terminal], AckProxies).

handle_ack(Sequence, From, #ping{sequence=Sequence, terminal=From} = Ping, State) ->
    #state{membership=Membership} = State,
    #ping{tref=TRef, incarnation=Incarnation} = Ping,
    _ = erlang:cancel_timer(TRef),
    _ = swim_membership:alive(Membership, From, Incarnation),
    State#state{current_ping=undefined};
handle_ack(Sequence, From, _CurrentPing, State) ->
    #state{proxy_pings=ProxyPings} = State,
    case lists:keytake(From, #ping.terminal, ProxyPings) of
	{value, Ping, NewProxyPings} ->
	    #ping{origin=Origin} = Ping,
	    ok = send_ack(Origin, Sequence, From, State),
	    State#state{proxy_pings=NewProxyPings};
	false ->
	    State
    end.

send_ack({Ip, Port}, Sequence, From, State) ->
    #state{transport=Transport, membership=Membership,
	  broadcasts=Broadcasts} = State,
    NumMembers = swim_membership:num_members(Membership),
    Events = swim_broadcasts:dequeue(Broadcasts, NumMembers),
    Msg = swim_messages:encode_ack(Sequence, From, Events),
    ok = swim_transport:send(Transport, Ip, Port, Msg),
    ok.

send_ping_req({Ip, Port}, Terminal, Sequence, State) ->
    #state{transport=Transport} = State,
    Msg = swim_messages:encode_ping_req(Sequence, Terminal),
    ok = swim_transport:send(Transport, Ip, Port, Msg).

send_ping({{Ip, Port} = To, Incarnation}, From, Sequence, State) ->
    #state{transport=Transport, membership=Membership,
	  broadcasts=Broadcasts} = State,
    NumMembers = swim_membership:num_members(Membership),
    Events = swim_broadcasts:dequeue(Broadcasts, NumMembers),
    Msg = swim_messages:encode_ping(Sequence, Events),
    ok = swim_transport:send(Transport, Ip, Port, Msg),
    Ref = make_ref(),
    TRef = create_ack_timer(Ref, State),
    Ping = #ping{sequence=Sequence,
		 incarnation=Incarnation,
		 origin=From, terminal=To, ref=Ref,
		 tref=TRef, sent=erlang:monotonic_time()},
    {ok, Ping}.

handle_ping_req(Sequence, {Ip, Port} = Terminal, Origin, State) ->
    #state{proxy_pings=ProxyPings,
	   membership=Membership,
	   broadcasts=Broadcasts,
	   transport=Transport} = State,
    Ping = #ping{origin=Origin, terminal=Terminal},
    NumMembers = swim_membership:num_members(Membership),
    Events = swim_broadcasts:dequeue(Broadcasts, NumMembers),
    Msg = swim_messages:encode_ping(Sequence, Events),
    ok = swim_transport:send(Transport, Ip, Port, Msg),
    State#state{proxy_pings=[Ping | ProxyPings]}.

handle_ping(Sequence, From, State) ->
    #state{local_member=LocalMember, membership=Membership} = State,
    _ = swim_membership:alive(Membership, From, 0),
    ok = send_ack(From, Sequence, LocalMember, State),
    State.

handle_events([], _State) ->
    [];
handle_events(Events, State) ->
    lists:flatten([handle_event(Event, State) || Event <- Events]).

handle_event({membership, {alive, Member, Incarnation}}, State) ->
    #state{membership=Membership} = State,
    swim_membership:alive(Membership, Member, Incarnation);
handle_event({membership, {suspect, Member, Incarnation}}, State) ->
    #state{membership=Membership} = State,
    swim_membership:suspect(Membership, Member, Incarnation);
handle_event({membership, {faulty, Member, Incarnation}}, State) ->
    #state{membership=Membership} = State,
    swim_membership:faulty(Membership, Member, Incarnation);
handle_event({user, Event}, State) ->
    #state{owner=Owner} = State,
    Owner ! {swim, {user, Event}},
    ok.
