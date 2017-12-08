-module(throttle_distributed_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

%% FIXME test with async_dirty, transaction and sync_transaction
groups() ->
  [{distributed, [{repeat, 10}], [test_limit]}].

all() ->
[{group, distributed}].

init_per_suite(Config) ->
  {ok, Slave} = start_slave(),
  start_throttle(),
  [{slave, Slave} | Config].

end_per_suite(Config) ->
  Slave = proplists:get_value(slave, Config),
  ct_slave:stop(Slave),
  application:stop(throttle),
  ok.

start_slave() ->
  Slave = 'node2@127.0.0.1',

  %% make the same binaries available to the other node
  CodePath = code:get_path(),
  PathFlag = "-pa " ++ lists:concat(lists:join(" ", CodePath)),
  {ok, _} = ct_slave:start(Slave, [{erl_flags, PathFlag}]),

  ok = rpc:call(Slave, throttle_distributed_SUITE, start_throttle, []),
  {ok, Slave}.

start_throttle() ->
  application:ensure_all_started(lager),
  ok = application:set_env(throttle, driver, throttle_mnesia),
  ok = application:set_env(throttle, access_context, ets),
  {ok, _Apps} = application:ensure_all_started(throttle),
  throttle_mnesia:init(),
  ok.

test_limit(Config) ->
  Slave = proplists:get_value(slave, Config),
  Scope = binary_to_atom(integer_to_binary(rand:uniform(10000)), latin1),
  throttle:setup(Scope, 4, per_second),

  %% check both here and in the other node
  {ok, 3, _} = throttle:check(Scope, <<"john">>),
  {ok, 2, _} = rpc:call(Slave, throttle, check, [Scope, <<"john">>]),
  {ok, 1, _} = throttle:check(Scope, <<"john">>),
  {ok, 0, _} = rpc:call(Slave, throttle, check, [Scope, <<"john">>]),
  {limit_exceeded, 0, _} = throttle:check(Scope, <<"john">>),

  timer:sleep(1100),

  {ok, 3, _} = throttle:check(Scope, <<"john">>),

  ok.
