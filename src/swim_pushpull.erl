-module(swim_pushpull).
-behavior(gen_server).

-export([join/2, join/3]).
-export([start_link/3, start_link/4]).
-export([accept/5]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-record(state, {
          name         :: atom(),
          socket       :: swim_socket:stream_socket(),
          acceptors    :: ets:tab(),
          local_member :: swim:member(),
          opts         :: map()
         }).

join(Member, Opts) ->
    join(default, Member, Opts).

join(Name, Member, Opts) ->
    StateRef = swim_name:proc_name(Name, state),
    LocalMember = swim_state:local_member(StateRef),
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
                    merge_state(Name, RemoteState),
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
    start_link(default, IpAddr, Port, Opts).

start_link(Name, IpAddr, Port, Opts) ->
    gen_server:start_link(?MODULE, [Name, IpAddr, Port, Opts], []).

init([Name, IpAddr, Port, Opts]) ->
    MinAcceptors = maps:get(min_acceptors, Opts, 2),
    TcpOpts = [binary, {packet, 4}, {ip, IpAddr},
               {reuseaddr, true}, {nodelay, true},
               {active, false}],
    {ok, Socket} = swim_socket:listen(tcp, Port, TcpOpts),
    Acceptors = ets:new(acceptor, [private, set]),
    State = #state{name = Name, local_member = {IpAddr, Port}, socket = Socket,
                   acceptors = Acceptors, opts = Opts},
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
    Args = [State#state.name, self(), State#state.local_member,
            State#state.socket, State#state.opts],
    Pid = spawn_link(?MODULE, accept, Args),
    ets:insert(State#state.acceptors, {Pid}),
    ok.

remove_acceptor(State, Pid) ->
    ets:delete(State#state.acceptors, Pid),
    ok.

accept(Name, Server, LocalMember, ListenSocket, Opts) ->
    Result =
        try swim_socket:accept(ListenSocket, Server, maps:get(accept_timeout, Opts, 10000))
        catch Class:CatchReason -> {'EXIT', {Class, CatchReason}}
        end,
    case Result of
        {ok, Socket} ->
            read_message(Name, LocalMember, Socket, Opts),
            swim_socket:close(Socket),
            ok;
        {error, timeout} ->
            accept(Name, Server, LocalMember, ListenSocket, Opts);
        {error, econnaborted} ->
            accept(Name, Server, LocalMember, ListenSocket, Opts);
        {error, {tls_alert, _}} ->
            accept(Name, Server, LocalMember, ListenSocket, Opts);
        {error, closed} ->
            ok;
        {error, Reason} ->
            exit({error, Reason})
    end.

read_message(Name, LocalMember, Socket, Opts) ->
    case swim_socket:recv(Socket, 0, maps:get(receive_timeout, Opts, 60000)) of
        {ok, Data} ->
            handle_message(Name, decode(Data), LocalMember, Socket);
        {error, Reason} ->
            {error, Reason}
    end.

handle_message(Name, {push_pull, RemoteMember, RemoteState}, LocalMember, Socket) ->
    StateRef = swim_name:proc_name(Name, state),
    LocalState = swim_state:local_state(StateRef),
    send_message({push_pull, LocalMember, LocalState}, Socket),
    swim_metrics:notify(swim_name:proc_name(Name, metrics), {push_pull, RemoteMember}),
    spawn_link(fun() -> merge_state(Name, RemoteState) end),
    ok;
handle_message(_Name, _Other, _LocalMember, _Socket) ->
    ok.

send_message(Message, Socket) ->
    EncodedMessage = encode(Message),
    swim_socket:send(Socket, EncodedMessage).

decode(Data) ->
    binary_to_term(Data).

encode(Data) ->
    term_to_binary(Data).

merge_state(Name, RemoteState) ->
    StateRef = swim_name:proc_name(Name, state),
    [swim_state:handle_event(StateRef, Event) || Event <- RemoteState].
