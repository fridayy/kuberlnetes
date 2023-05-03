.PHONY: all
all: build

fmt:
	rebar3 fmt

check-types:
	rebar3 gradualizer

check-fmt:
	rebar3 fmt --check

compile:
	rebar3 compile

ct:
	rebar3 as test ct -v

eunit:
	rebar3 as test eunit

.PHONY: test
test: eunit ct

.PHONY: clean
clean:
	rebar3 clean

build: check-fmt compile test
