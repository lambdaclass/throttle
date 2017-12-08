.PHONY: dev test

dev:
	./rebar3 compile && ./rebar3 shell

test:
	./rebar3 ct --name node1@127.0.0.1
