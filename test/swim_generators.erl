-module(swim_generators).

-include_lib("proper/include/proper.hrl").

-compile([export_all]).

ip_address() ->
    oneof([
           tuple([range(0, 255) || _ <- lists:seq(1, 4)]),
           tuple([range(0, 65535) || _ <- lists:seq(1, 8)])
          ]).

port_number() ->
    integer(0, 65535).

member_status() ->
    oneof([alive, suspect, faulty]).

incarnation() ->
    integer(0, 1 bsl 32).

membership_event() ->
    ?LET({Status, Incarnation, Member},
         {member_status(), incarnation(), member()},
         {membership, {Status, Incarnation, Member}}).

user_event() ->
    ?LET(Bin, binary(), {user, Bin}).

swim_event() ->
    oneof([user_event(), membership_event()]).

sequence() ->
    integer(0, 1 bsl 32).

member() ->
    tuple([ip_address(), port_number()]).

swim_events() ->
    ?SIZED(Size, swim_events(Size)).

swim_events(Size) when Size > 256 ->
    resize(round(Size / 2), list(swim_event()));
swim_events(_Size) ->
    list(swim_event()).

ack() ->
    ?LET({Seq, Target, Events}, {sequence(), member(), swim_events()},
         {ack, Seq, Target, Events}).

nack() ->
    ?LET({Seq, Target, Events}, {sequence(), member(), swim_events()},
         {nack, Seq, Target, Events}).

ping() ->
    ?LET({Seq, Target, Events}, {sequence(), member(), swim_events()},
         {ping, Seq, Target, Events}).

ping_req() ->
    ?LET({Seq, Target, Events}, {sequence(), member(), swim_events()},
         {ping_req, Seq, Target, Events}).

swim_message() ->
    oneof([ack(), ping(), ping_req(), nack()]).
