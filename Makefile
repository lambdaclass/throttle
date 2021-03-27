.PHONY: dev test

dev:
	./rebar3 compile && ./rebar3 shell

test:
	./rebar3 ct --name node1@127.0.0.1 ; ./rebar3 dialyzer

travis_test:
	./rebar3 ct --name node1@127.0.0.1 ; ./rebar3 as test coveralls send ; ./rebar3 dialyzer
