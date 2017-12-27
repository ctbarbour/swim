-module(swim_pushpull).
-behavior(gen_server).

-export([join/3]).

-export([start_link/4]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-record(state, {
          socket     :: inet:socket(),
          transport  :: module()
         }).

-define(DATA_MSG(Tag), Tag == tcp orelse Tag == ssl).
-define(ERROR_MSG(Tag), Tag == tcp_error orelse Tag == ssl_error).
-define(CLOSED_MSG(Tag), Tag == tcp_closed orelse Tag == ssl_closed).

join(Member, LocalMember, Opts) ->
    Transport = case maps:get(transport, Opts, tcp) of
                    tcp -> ranch_tcp;
                    ssl -> ranch_ssl
                end,
    TransportOpts = [binary, {active, false} | maps:get(transport_opts, Opts, [])],
    case connect(Member, Transport, TransportOpts, Opts, maps:get(retries, Opts, 5)) of
        {ok, Socket} ->
            EncodedMessage = encode({push_pull, [{membership, {alive, 0, LocalMember}}]}),
            ok = Transport:send(Socket, EncodedMessage),
            case Transport:recv(Socket, 0, 5000) of
                {ok, Data} ->
                    Transport:close(Socket),
                    {push_pull, RemoteState} = decode(Data),
                    merge_state(RemoteState),
                    ok;
                Error ->
                    Error
            end;
        Err ->
            Err
    end.

connect({Ip, Port} = Member, Transport, TransportOpts, Opts, Retries) ->
    case Transport:connect(Ip, Port, TransportOpts, maps:get(connect_timeout, Opts, 5000)) of
        {ok, Socket} ->
            {ok, Socket};
        {error, _Reason} ->
            retry_connect(Member, Transport, TransportOpts, Opts, Retries)
    end.

retry_connect(_Member, _Transport, _TransportOpts, _Opts, 0) ->
    {error, retry_limit_exceeded};
retry_connect(Member, Transport, TransportOpts, Opts, Retries) ->
    _ = erlang:send_after(maps:get(retry_timeout, Opts, 5000), self(), retry),
    receive
        retry ->
            connect(Member, Transport, TransportOpts, Opts, Retries - 1)
    end.

start_link(Ref, Socket, Transport, Opts) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Socket, Transport, Opts}])}.

init({Ref, Socket, Transport, _Opts}) ->
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    gen_server:enter_loop(?MODULE, [], #state{socket = Socket, transport = Transport}).

handle_call(_Req, _From, State) ->
    {noreply, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info({Tag, Socket, Data}, #state{socket = Socket, transport = Transport} = State)
  when ?DATA_MSG(Tag) ->
    handle_message(decode(Data), State),
    ok = Transport:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_info({Tag, Socket}, #state{socket = Socket} = State)
  when ?CLOSED_MSG(Tag) ->
    {stop, normal, State};
handle_info({Tag, Socket, Reason}, #state{socket = Socket} = State)
  when ?ERROR_MSG(Tag) ->
    {stop, Reason, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

handle_message({push_pull, RemoteState}, State) ->
    LocalState = swim_state:local_state(),
    send_message({push_pull, LocalState}, State),
    spawn_link(fun() -> merge_state(RemoteState) end),
    ok;
handle_message(_Other, _State) ->
    ok.

send_message(Message, #state{socket = Socket, transport = Transport}) ->
    EncodedMessage = encode(Message),
    Transport:send(Socket, EncodedMessage).

decode(Data) ->
    binary_to_term(Data).

encode(Data) ->
    term_to_binary(Data).

merge_state(RemoteState) ->
    [swim_state:handle_event(Event) || Event <- RemoteState].
