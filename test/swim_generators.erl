-module(swim_generators).

-include_lib("proper/include/proper.hrl").

-compile([export_all]).

g_ip_address() ->
    ip_address().

ip_address() ->
    oneof([
           tuple([range(0, 255) || _ <- lists:seq(1, 4)]),
           tuple([range(0, 65535) || _ <- lists:seq(1, 8)])
          ]).

g_port_number() ->
    port_number().

port_number() ->
    range(0, 65535).

g_incarnation() ->
    incarnation().

incarnation() ->
    range(0, 1 bsl 32).

g_membership_event() ->
    membership_event().

membership_event() ->
    ?LET(Event,
         oneof([suspect_event(), alive_event(), faulty_event()]),
         {membership, Event}).

g_suspect_event() ->
    suspect_event().

suspect_event() ->
    ?LET({Incarnation, Member, From},
         {incarnation(), member(), member()},
         {suspect, Incarnation, Member, From}).

g_alive_event() ->
    alive_event().

alive_event() ->
    ?LET({Incarnation, Member},
         {incarnation(), member()},
         {alive, Incarnation, Member}).

g_faulty_event() ->
    faulty_event().

faulty_event() ->
    ?LET({Incarnation, Member, From},
         {incarnation(), member(), member()},
         {faulty, Incarnation, Member, From}).

g_user_event() ->
    user_event().

user_event() ->
    ?LET(Bin, binary(), {user, Bin}).

g_swim_event() ->
    swim_event().

swim_event() ->
    oneof([user_event(), membership_event()]).

g_sequence() ->
    sequence().

sequence() ->
    range(0, 1 bsl 32).

g_member() ->
    member().

member() ->
    tuple([ip_address(), port_number()]).

g_swim_events() ->
    swim_events().

swim_events() ->
    ?SIZED(Size, swim_events(Size)).

swim_events(Size) when Size > 256 ->
    resize(round(Size / 2), list(swim_event()));
swim_events(_Size) ->
    list(swim_event()).

g_ack() ->
    ack().

ack() ->
    ?LET({Seq, Target, Events}, {sequence(), member(), swim_events()},
         {{ack, Seq, Target}, Events}).

g_nack() ->
    nack().

nack() ->
    ?LET({Seq, Target, Events}, {sequence(), member(), swim_events()},
         {{nack, Seq, Target}, Events}).

g_ping() ->
    ping().

ping() ->
    ?LET({Seq, Target, Events}, {sequence(), member(), swim_events()},
         {{ping, Seq, Target}, Events}).

g_ping_req() ->
    ping_req().

ping_req() ->
    ?LET({Seq, Target, Events}, {sequence(), member(), swim_events()},
         {{ping_req, Seq, Target}, Events}).

g_swim_message() ->
    swim_message().

swim_message() ->
    oneof([ack(), ping(), ping_req(), nack()]).
