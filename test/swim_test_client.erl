-module(swim_test_client).
-behavior(gen_server).

-export([start/2]).
-export([stop/1]).
-export([call/2]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

-record(state, {
          local_member,
          keyring,
          socket,
          requests
         }).

start(LocalMember, Key) ->
    gen_server:start(?MODULE, [LocalMember, Key], []).

stop(Pid) ->
    gen_server:stop(Pid).

call(Pid, Msg) ->
    try
        gen_server:call(Pid, Msg, 500)
    catch
        _:_ ->
            timeout
    end.

init([{_, Port} = LocalMember, Key]) ->
    Keyring = swim_keyring:new([Key]),
    {ok, Socket} = gen_udp:open(Port, [binary, {active, true}]),
    {ok, #state{requests = #{}, socket = Socket, keyring = Keyring, local_member = LocalMember}}.

handle_call({ping, Sequence, {Ip, Port} = Target}, From, State) ->
    Msg = swim_messages:encode({{ping, Sequence, Target}, []}),
    Payload = swim_keyring:encrypt(Msg, State#state.keyring),
    ok = gen_udp:send(State#state.socket, Ip, Port, Payload),
    {noreply, State#state{requests = maps:put(Sequence, From, State#state.requests)}};
handle_call({ping_req, Sequence, {Ip, Port}, Terminal}, From, State) ->
    Msg = swim_messages:encode({{ping_req, Sequence, Terminal}, []}),
    Payload = swim_keyring:encrypt(Msg, State#state.keyring),
    ok = gen_udp:send(State#state.socket, Ip, Port, Payload),
    {noreply, State#state{requests = maps:put(Sequence, From, State#state.requests)}}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info({udp, _Socket, Ip, InPortNo, Packet}, State) ->
    case swim_keyring:decrypt(Packet, State#state.keyring) of
        {ok, PlainText} ->
            try
                {Message, _Events} = swim_messages:decode(PlainText),
                case Message of
                    {ack, Sequence, _Target} = Reply ->
                        case maps:take(Sequence, State#state.requests) of
                            {From, Requests} ->
                                gen_server:reply(From, Reply),
                                {noreply, State#state{requests = Requests}};
                            error ->
                                {noreply, State}
                        end;
                    {ping, Sequence, Target} ->
                        Ack = swim_messages:encode({{ack, Sequence, Target}, []}),
                        Payload = swim_keyring:encrypt(Ack, State#state.keyring),
                        ok = gen_udp:send(State#state.socket, Ip, InPortNo, Payload),
                        {noreply, State};
                    _ ->
                        {noreply, State}
                end
            catch
                _:_ ->
                    {noreply, State}
            end;
        {error, failed_verification} ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
