-module(swim_probe).
-behavior(gen_statem).

-include("swim.hrl").

-export([start_link/5]).
-export([start_link/7]).
-export([ack/3]).
-export([nack/3]).

-export([init/1]).
-export([callback_mode/0]).
-export([code_change/4]).
-export([terminate/3]).

-export([direct/3]).
-export([indirect/3]).
-export([handle_event/3]).

-record(data, {
          origin   :: member(),
          terminal :: member(),
          sequence :: non_neg_integer(),
          proxies  :: [member()],
          timeout  :: timeout(),
          ack_fun  :: fun((member(), sequence()) -> ok),
          nack_fun :: fun((member(), sequence()) -> ok)
         }).

start_link(Origin, Target, Sequence, Proxies, Timeout) ->
    AckFun = fun(Member, Seq) -> swim_state:ack(Member, Seq) end,
    NackFun = fun(Member, Seq) -> swim_state:nack(Member, Seq) end,
    start_link(Origin, Target, Sequence, Proxies, Timeout, AckFun, NackFun).

start_link(Origin, Target, Sequence, Proxies, Timeout, AckFun, NackFun) ->
    Args = [Origin, Target, Sequence, Proxies, Timeout, AckFun, NackFun],
    gen_statem:start_link(?MODULE, Args, []).

ack(Pid, Member, Sequence) ->
    gen_statem:cast(Pid, {ack, Member, Sequence}).

nack(Pid, Member, Sequence) ->
    gen_statem:cast(Pid, {nack, Member, Sequence}).

callback_mode() ->
    [state_events, state_enter].

init([Origin, Target, Sequence, Proxies, Timeout, AckFun, NackFun]) ->
    Probe = #data{
               origin   = Origin,
               terminal = Target,
               sequence = Sequence,
               proxies  = Proxies,
               timeout  = Timeout,
               ack_fun  = AckFun,
               nack_fun = NackFun
              },
    {ok, direct, Probe}.

direct(enter, _OldState, Data) ->
    #data{origin = Origin, terminal = Terminal, sequence = Sequence} = Data,
    ok = swim_net:ping(Terminal, Origin, Sequence),
    Actions = [{state_timeout, Data#data.timeout, ack_timeout}],
    {keep_state_and_data, Actions};
direct(state_timeout, ack_timeout, Data) ->
    {next_state, indirect, Data};
direct(EventType, EventContext, Data) ->
    handle_event(EventType, EventContext, Data).

indirect(enter, _OldState, Data) ->
    #data{proxies = Proxies, terminal = Terminal, origin = Origin, sequence = Sequence} = Data,
    _ = [swim_net:ping_req(Proxy, Terminal, Origin, Sequence) || Proxy <- Proxies],
    {keep_state, Data, [{state_timeout, Data#data.timeout, probe_timeout}]};
indirect(state_timeout, probe_timeout, Data) ->
    {stop, normal, Data};
indirect(cast, {nack, Terminal}, _Data) ->
    ok = swim_state:nack(Terminal),
    keep_state_and_data;
indirect(EventType, EventContext, Data) ->
    handle_event(EventType, EventContext, Data).

handle_event(cast, {ack, Responder, Sequence}, #data{ack_fun = AckFun} = Data)
  when Responder =:= Data#data.terminal ->
    ok = AckFun(Responder, Sequence),
    {stop, normal, Data};
handle_event(_EventType, _EventContext, _Data) ->
    keep_state_and_data.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

terminate(_Reason, _State, _Data) ->
    ok.
