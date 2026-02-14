.PHONY: test compile fmt check

test:
	rebar3 eunit

compile:
	rebar3 compile

fmt:
	rebar3 fmt

check:
	rebar3 fmt --check
	rebar3 xref
	rebar3 dialyzer
