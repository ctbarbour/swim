-module(swim_gossip_v2).

-behavior(gen_server).

-include("swim.hrl").

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
	 terminate/2]).

-record(state, {
	  protocol_period = 3000 :: pos_integer(),
	  ack_proxies     = 3    :: pos_integer(),
	  ack_timeout     = 750 :: pos_integer(),
	  current_ping           :: ping(),
	  local_member           :: member(),
	  proxy_pings     = []   :: [ping()],
	  ping_targets    = []   :: [{member(), incarnation()}],
	  sequence        = 0    :: non_neg_integer(),
	  membership             :: pid(),
	  transport              :: pid()
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

start_link(Membership, TransportOpts, Opts) ->
    gen_server:start_link(?MODULE, [Membership, TransportOpts, Opts], []).

start_transport(Opts) ->
    Ip = proplists:get_value(ip, Opts),
    Port = proplists:get_value(port, Opts),
    Keys = proplists:get_value(keys, Opts, []),
    swim_transport:start_link(Ip, Port, Keys).

init([Membership, TransportOpts, Opts]) ->
    State = handle_opts(Opts, #state{membership=Membership}),
    {ok, Transport} = start_transport(TransportOpts),
    self() ! protocol_period,
    {ok, State#state{transport=Transport}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(protocol_period, State) ->
    NewState = handle_protocol_period(State),
    ok = schedule_next_protocol_period(NewState),
    {noreply, NewState};
handle_info({ack_timeout, Ref}, State) ->
    NewState = handle_ack_timeout(Ref, State),
    {noreply, NewState};
handle_info({{ack, Sequence, Responder, Events}, From}, State) ->
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
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

handle_opts([], State) ->
    State;
handle_opts([{protocol_period, Val} | Rest], State)
  when is_integer(Val) andalso Val > 0 ->
    handle_opts(Rest, State#state{protocol_period=Val});
handle_opts([{ack_proxies, Val} | Rest], State)
  when is_integer(Val) andalso Val > 0 ->
    handle_opts(Rest, State#state{ack_proxies=Val});
handle_opts([{ack_timeout, Val} | Rest], State)
  when is_integer(Val) andalso Val > 0 ->
    handle_opts(Rest, State#state{ack_timeout=Val});
handle_opts([_ | Rest], State) ->
    handle_opts(Rest, State).

handle_protocol_period(#state{current_ping=undefined} = State) ->
    send_next_ping(State);
handle_protocol_period(#state{current_ping=Ping} = State) ->
    #state{membership=Membership} = State,
    #ping{terminal=Terminal, incarnation=Incarnation} = Ping,
    ok = swim_membership_v2:suspect(Membership, Terminal, Incarnation),
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
    Members = swim_membership_v2:members(Membership),
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
    lists:sublist([M || {M, _I} = P <- ping_targets(State),
			 M /= Terminal], AckProxies).

handle_ack(Sequence, From, #ping{sequence=Sequence, terminal=From} = Ping, State) ->
    #state{membership=Membership} = State,
    #ping{tref=TRef, incarnation=Incarnation} = Ping,
    _ = erlang:cancel_timer(TRef),
    ok = swim_membership_v2:alive(Membership, From, Incarnation),
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
    #state{transport=Transport, membership=Membership} = State,
    Events = swim_membership_v2:events(Membership),
    Msg = swim_messages:encode_ack(Sequence, From, Events),
    ok = swim_transport:send(Transport, Ip, Port, Msg),
    ok.

send_ping_req({Ip, Port}, Terminal, Sequence, State) ->
    #state{transport=Transport} = State,
    Msg = swim_messages:encode_ping_req(Sequence, Terminal),
    ok = swim_transport:send(Transport, Ip, Port, Msg).

send_ping({{Ip, Port} = To, Incarnation}, From, Sequence, State) ->
    #state{transport=Transport, membership=Membership} = State,
    Events = swim_membership_v2:events(Membership),
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
    #state{proxy_pings=ProxyPings, membership=Membership,
	   transport=Transport} = State,
    Ping = #ping{origin=Origin, terminal=Terminal},
    Events = swim_membership_v2:events(Membership),
    Msg = swim_messages:encode_ping(Sequence, Events),
    ok = swim_transport:send(Transport, Ip, Port, Msg),
    State#state{proxy_pings=[Ping | ProxyPings]}.

handle_ping(Sequence, From, State) ->
    #state{membership=Membership} = State,
    ok = swim_membership_v2:alive(Membership, From, 0),
    Me = swim_membership_v2:local_member(Membership),
    ok = send_ack(From, Sequence, Me, State),
    State.

handle_events([], State) ->
    State;
handle_events(Events, State) ->
    [handle_event(Event, State) || Event <- Events].

handle_event({membership, {alive, Member, Incarnation}}, State) ->
    #state{membership=Membership} = State,
    ok = swim_membership_v2:alive(Membership, Member, Incarnation),
    State;
handle_event({membership, {suspect, Member, Incarnation}}, State) ->
    #state{membership=Membership} = State,
    ok = swim_membership_v2:suspect(Membership, Member, Incarnation),
    State;
handle_event({membership, {faulty, Member, Incarnation}}, State) ->
    #state{membership=Membership} = State,
    ok = swim_membership_v2:faulty(Membership, Member, Incarnation),
    State;
handle_event({user, Event}, State) ->
    ok = error_logger:info_msg("User event ~p", [Event]),
    State.
