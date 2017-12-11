.PHONY: dev test

dev:
	./rebar3 compile && ./rebar3 shell

test:
	./rebar3 ct --name node1@127.0.0.1

travis_test:
	./rebar3 ct --name node1@127.0.0.1 ; DEBUG=1 ./rebar3 coveralls send
