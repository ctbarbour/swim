-module(swim_pushpull).
-behavior(gen_server).

-export([join/2]).

-export([start_link/3]).
-export([accept/4]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-record(state, {
          socket       :: inet:socket() | ssl:socket(),
          acceptors    :: ets:tab(),
          local_member :: swim:member(),
          opts         :: maps:map()
         }).

join(Member, Opts) ->
    LocalMember = swim_state:local_member(),
    Transport = maps:get(transport, Opts, tcp),
    TransportOpts = [binary, {packet, 4}, {active, false}, {nodelay, true}
                     | maps:get(transport_opts, Opts, [])],
    Retries = maps:get(retries, Opts, 5),
    case connect(Member, Transport, TransportOpts, Opts, Retries) of
        {ok, Socket} ->
            Msg = {push_pull, LocalMember, [{membership, {alive, 0, LocalMember}}]},
            ok = swim_socket:send(Socket, encode(Msg)),
            case swim_socket:recv(Socket, 0, 5000) of
                {ok, Data} ->
                    swim_socket:close(Socket),
                    {push_pull, _RemoteMember, RemoteState} = decode(Data),
                    merge_state(RemoteState),
                    ok;
                Error ->
                    Error
            end;
        Err ->
            Err
    end.

connect({Ip, Port} = Member, Transport, TransportOpts, Opts, Retries) ->
    ConnectTimeout = maps:get(connect_timeout, Opts, 5000),
    case swim_socket:connect(Transport, Ip, Port, TransportOpts, ConnectTimeout)of
        {ok, Socket} ->
            {ok, Socket};
        {error, _Reason} ->
            retry_connect(Member, Transport, TransportOpts, Opts, Retries)
    end.

retry_connect(_Member, _Transport, _TransportOpts, _Opts, 0) ->
    {error, retry_limit_exceeded};
retry_connect(Member, Transport, TransportOpts, Opts, Retries) ->
    RetryTimeout = maps:get(retry_timeout, Opts, 5000),
    _ = erlang:send_after(RetryTimeout, self(), retry),
    receive
        retry ->
            connect(Member, Transport, TransportOpts, Opts, Retries - 1)
    end.

start_link(IpAddr, Port, Opts) ->
    gen_server:start_link(?MODULE, [IpAddr, Port, Opts], []).

init([IpAddr, Port, Opts]) ->
    MinAcceptors = maps:get(min_acceptors, Opts, 2),
    TcpOpts = [binary, {packet, 4}, {ip, IpAddr},
               {reuseaddr, true}, {nodelay, true},
               {active, false}],
    {ok, Socket} = swim_socket:listen(tcp, Port, TcpOpts),
    Acceptors = ets:new(accecptor, [private, set]),
    State = #state{socket = Socket, acceptors = Acceptors, opts = Opts},
    [start_add_acceptor(State) || _ <- lists:seq(1, MinAcceptors)],
    {ok, State}.

handle_call(_Req, _From, State) ->
    {noreply, State}.

handle_cast(accepted, State) ->
    ok = start_add_acceptor(State),
    {noreply, State};
handle_cast(_Req, State) ->
    {noreply, State}.

handle_info({'EXIT', _Pid, {error, emfile}}, State) ->
    {stop, emfile, State};
handle_info({'EXIT', Pid, _Reason}, State) ->
    ok = remove_acceptor(State, Pid),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

start_add_acceptor(State) ->
    Args = [self(), State#state.local_member, State#state.socket, State#state.opts],
    Pid = spawn_link(?MODULE, accept, Args),
    ets:insert(State#state.acceptors, {Pid}),
    ok.

remove_acceptor(State, Pid) ->
    ets:delete(State#state.acceptors, Pid),
    ok.

accept(Server, LocalMember, ListenSocket, Opts) ->
    case catch swim_socket:accept(ListenSocket, Server, maps:get(accept_timeout, Opts, 10000)) of
        {ok, Socket} ->
            read_message(LocalMember, Socket, Opts),
            swim_socket:close(Socket),
            ok;
        {error, timeout} ->
            accept(Server, LocalMember, ListenSocket, Opts);
        {error, econnaborted} ->
            accept(Server, LocalMember, ListenSocket, Opts);
        {error, {tls_alert, _}} ->
            accept(Server, LocalMember, ListenSocket, Opts);
        {error, closed} ->
            ok;
        {error, Reason} ->
            exit({error, Reason})
    end.

read_message(LocalMember, Socket, Opts) ->
    case swim_socket:recv(Socket, 0, maps:get(receive_timeout, Opts, 60000)) of
        {ok, Data} ->
            handle_message(decode(Data), LocalMember, Socket);
        {error, Reason} ->
            {error, Reason}
    end.

handle_message({push_pull, RemoteMember, RemoteState}, LocalMember, Socket) ->
    LocalState = swim_state:local_state(),
    send_message({push_pull, LocalMember, LocalState}, Socket),
    swim_metrics:notify({push_pull, RemoteMember}),
    spawn_link(fun() -> merge_state(RemoteState) end),
    ok;
handle_message(_Other, _LocalMember, _Socket) ->
    ok.

send_message(Message, Socket) ->
    EncodedMessage = encode(Message),
    swim_socket:send(Socket, EncodedMessage).

decode(Data) ->
    binary_to_term(Data).

encode(Data) ->
    term_to_binary(Data).

merge_state(RemoteState) ->
    [swim_state:handle_event(Event) || Event <- RemoteState].
