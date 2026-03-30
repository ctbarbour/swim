compile:
    rebar3 compile

check: compile
    rebar3 do xref, dialyzer

test:
    rebar3 ct
