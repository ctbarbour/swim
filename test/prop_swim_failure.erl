-module(prop_swim_failure).

-include_lib("proper/include/proper.hrl").

-behavior(proper_fsm).

-export([initial_state/0]).
-export([initial_state_data/0]).
-export([precondition/4]).
-export([postcondition/5]).
-export([next_state_data/5]).

-export([listen/1]).

-export([start/0]).
-export([stop/0]).

-export([start_client/0]).
-export([send_ping/1]).
-export([send_ping_req/1]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-define(KEY, <<"wGokGbCsoUhUCf9g9ivxUKZzrd8Qlb0JC7Jny8Er3ao=">>).
-define(LOCAL, {{127,0,0,1}, 9000}).
-define(CLIENT, {{127,0,0,1}, 9100}).

-record(data, {
          sequence
         }).

-record(state, {
          socket,
          keyring,
          pings,
          sequence
         }).

initial_state() ->
    listen.

initial_state_data() ->
    #data{}.

precondition(_From, _Target, _Data, {call, ?MODULE, _Fun, _Args}) ->
    true.

postcondition(listen, _Next, _Data, {call, ?MODULE, send_ping, [_Target]}, Result) ->
    Result =:= ok;
postcondition(listen, _Next, _Data, {call, ?MODULE, send_ping_req, [_Target]}, Result) ->
    Result =:= ok.

next_state_data(_From, _Target, Data, _Result, {call, ?MODULE, _Fun, _Args}) ->
    Data.

listen(_Data) ->
    [
     {history, {call, ?MODULE, send_ping, [exactly(?LOCAL)]}},
     {history, {call, ?MODULE, send_ping_req, [exactly(?CLIENT)]}}
    ].

prop_ping_ack() ->
    ?FORALL(Cmds, proper_fsm:commands(?MODULE),
            ?TRAPEXIT(
               begin
                   ok = start(),
                   {H, S, R} = proper_fsm:run_commands(?MODULE, Cmds),
                   ok = stop(),
                   ?WHENFAIL(
                      io:format("History: ~p~nState:~p~nResult:~p~n", [H, S, R]),
                      aggregate(command_names(Cmds), R =:= ok))
               end)).

start() ->
    ok = error_logger:tty(false),
    start_client(),
    application:set_env(swim, port, 9000),
    application:set_env(swim, key, ?KEY),
    application:start(swim).

stop() ->
    application:stop(swim),
    stop_client(),
    error_logger:tty(true).

start_client() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [element(2, ?CLIENT), ?KEY], []).

stop_client() ->
    gen_server:stop(?MODULE).

send_ping(Target) ->
    try
        gen_server:call(?MODULE, {ping, Target}, 500)
    catch
        error:_ ->
            timeout
    end.

send_ping_req(Target) ->
    try
        gen_server:call(?MODULE, {ping_req, Target}, 500)
    catch
        error:_ ->
            timeout
    end.

init([Port, Key]) ->
    Keyring = swim_keyring:new([base64:decode(Key)]),
    {ok, Socket} = gen_udp:open(Port, [binary, {active, true}]),
    {ok, #state{sequence = 0, socket = Socket, keyring = Keyring, pings = #{}}}.

handle_call({ping_req, Target}, From, State) ->
    Sequence = State#state.sequence + 1,
    Msg = swim_messages:encode({{ping_req, Sequence, Target}, []}),
    Payload = swim_keyring:encrypt(Msg, State#state.keyring),
    {Ip, Port} = ?LOCAL,
    ok = gen_udp:send(State#state.socket, Ip, Port, Payload),
    {noreply, State#state{sequence = Sequence,
                          pings = maps:put(Sequence, From, State#state.pings)}};
handle_call({ping, Target}, From, State) ->
    Sequence = State#state.sequence + 1,
    Payload = swim_keyring:encrypt(swim_messages:encode({{ping, Sequence, Target}, []}),
                                   State#state.keyring),
    {Ip, Port} = ?LOCAL,
    ok = gen_udp:send(State#state.socket, Ip, Port, Payload),
    {noreply, State#state{sequence = Sequence, pings = maps:put(Sequence, From, State#state.pings)}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({udp, Socket, Ip, InPortNo, Packet}, State)
  when State#state.socket =:= Socket ->
    NewState =
        case swim_keyring:decrypt(Packet, State#state.keyring) of
            {ok, PlainText} ->
                case swim_messages:decode(PlainText) of
                    {{ack, Sequence, _Target}, _} ->
                        case maps:take(Sequence, State#state.pings) of
                            {From, Pings} ->
                                gen_server:reply(From, ok),
                                State#state{pings = Pings};
                            error ->
                                State
                        end;
                    {{ping, Sequence, Target}, _} ->
                        Ack = swim_messages:encode({{ack, Sequence, Target}, []}),
                        Payload = swim_keyring:encrypt(Ack, State#state.keyring),
                        ok = gen_udp:send(Socket, Ip, InPortNo, Payload),
                        State;
                    _Msg ->
                        State
                end;
            _ ->
                State
        end,
    {noreply, NewState}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
