-module(swim_transport).
-behavior(gen_server).

-export([start_link/3, send/4, close/1, controlling_process/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
	 terminate/2]).

-record(state, {
	  parent :: pid(),
	  socket :: inet:socket(),
	  vault  :: pid()
	 }).

start_link(ListenIp, ListenPort, Keys) ->

    gen_server:start_link(?MODULE,
			  [self(), ListenIp, ListenPort, Keys], []).

close(Pid) ->
    gen_server:call(Pid, close).

controlling_process(Pid, Controller) ->
    gen_server:call(Pid, {controlling_process, self(), Controller}).

send(Pid, DestIp, DestPort, Data) ->
    gen_server:cast(Pid, {send, DestIp, DestPort, Data}).

init([Parent, ListenIp, ListenPort, Keys]) ->
    {ok, Vault} = swim_vault:start_link(Keys),
    SocketOpts = [binary, {ip, ListenIp}, {active, once}],
    ok = error_logger:info_msg("ListenIp: ~p ListPort: ~p", [ListenIp, ListenPort]),
    {ok, Socket} = gen_udp:open(ListenPort, SocketOpts),
    {ok, #state{parent=Parent, vault=Vault, socket=Socket}}.

handle_call(close, _From, State) ->
    #state{socket=Socket} = State,
    ok = gen_udp:close(Socket),
    {stop, normal, ok, State#state{socket=undefined}};
handle_call({controlling_process, Caller, Controller}, _From, #state{parent=Caller} = State) ->
    {reply, ok, State#state{parent=Controller}};
handle_call({controlling_process, Caller, _Controller}, _From, State) ->
    #state{parent=Parent} = State,
    {reply, {error, {Caller, Parent}}};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({send, DestIp, DestPort, Msg}, State) ->
    #state{socket=Socket, vault=Vault} = State,
    Encrypted = swim_vault:encrypt(Vault, Msg),
    ok = gen_udp:send(Socket, DestIp, DestPort, Encrypted),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({udp, Socket, Ip, InPortNo, Data}, #state{socket=Socket} = State) ->
    NewState = handle_udp(Data, {Ip, InPortNo}, State),
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state{socket=undefined}) ->
    ok;
terminate(_Reason, #state{socket=Socket}) ->
    ok = gen_udp:close(Socket),
    ok.

handle_udp(Data, From, State) ->
    #state{vault=Vault, parent=Parent} = State,
    case swim_vault:decrypt(Vault, Data) of
	{ok, SwimMessage} ->
	    case catch(swim_messages:decode(SwimMessage)) of
		{'EXIT', Reason} ->
		    ok = error_logger:warning_msg("Failed to decode message: ~p", [Reason]),
		    State;
		DecodedMessage ->
		    Parent ! {DecodedMessage, From},
		    State
	    end;
	{error, failed_verification} ->
	    ok = error_logger:warning_msg("Message failed verfication from ~p", [From]),
	    State
    end.
