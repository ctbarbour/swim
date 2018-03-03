REBAR ?= $(shell which rebar3)

.PHONY: test

compile:
	$(REBAR) do xref, dialyzer

test:
	$(REBAR) ct
