-module(swim_transport).
-behavior(gen_server).

-export([start_link/3, send/4, close/1, rotate_keys/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
	 terminate/2]).

-record(state, {
	  parent :: pid(),
	  socket :: inet:socket(),
	  keys = [] :: [binary()],
	  aad :: binary()
	 }).

start_link(ListenIp, ListenPort, Keys) ->
    gen_server:start_link(?MODULE,
			  [self(), ListenIp, ListenPort, Keys], []).

close(Pid) ->
    gen_server:call(Pid, close).

rotate_keys(Pid, Key) ->
    gen_server:cast(Pid, {rotate_keys, Key}).

send(Pid, DestIp, DestPort, Data) ->
    gen_server:cast(Pid, {send, DestIp, DestPort, Data}).

init([Parent, ListenIp, ListenPort, Keys]) ->
    SocketOpts = [binary, {ip, ListenIp}, {active, once}],
    {ok, Socket} = gen_udp:open(ListenPort, SocketOpts),
    {ok, #state{parent=Parent, keys=Keys, socket=Socket, aad=aad()}}.

handle_call(close, _From, State) ->
    #state{socket=Socket} = State,
    ok = gen_udp:close(Socket),
    {stop, normal, ok, State#state{socket=undefined}};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({rotate_keys, Key}, State) ->
    #state{keys=Keys} = State,
    {noreply, State#state{keys=[Key | Keys]}};
handle_cast({send, DestIp, DestPort, Msg}, State) ->
    #state{socket=Socket} = State,
    Encrypted = encrypt(Msg, State),
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
    #state{parent=Parent, keys=Keys, aad=AAD} = State,
    case decrypt(Data, AAD, Keys) of
	{ok, SwimMessage} ->
	    case catch(swim_messages:decode(SwimMessage)) of
		{'EXIT', _Reason} ->
		    State;
		DecodedMessage ->
		    Parent ! {DecodedMessage, From},
		    State
	    end;
	{error, failed_verification} ->
	    State
    end.

encrypt(Msg, State) ->
    #state{keys=[Key|_], aad=AAD} = State,
    swim_messages:encrypt(Key, AAD, Msg).

decrypt(_Data, _AAD, []) ->
    {error, failed_verification};
decrypt(Data, AAD, [Key | Keys]) ->
    case swim_messages:decrypt(Key, AAD, Data) of
	{error, failed_verification} ->
	    decrypt(Data, AAD, Keys);
	SwimMessage ->
	    {ok, SwimMessage}
    end.

aad() ->
    crypto:hash(sha256, term_to_binary(erlang:get_cookie())).
