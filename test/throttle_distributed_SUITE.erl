-module(throttle_distributed_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

groups() ->
  %% repeat to avoid "lucky" successes
  [{async_dirty, [{repeat, 5}], [test_limit]},
   {transaction, [{repeat, 5}], [test_limit]},
   {sync_transaction, [{repeat, 5}], [test_limit]}
  ].

all() ->
  [{group, async_dirty},
   {group, transaction},
   {group, sync_transaction}].

init_per_suite(Config) ->
  {ok, Slave} = start_slave(),
  [{slave, Slave} | Config].

end_per_suite(Config) ->
  Slave = proplists:get_value(slave, Config),
  ct_slave:stop(Slave),
  ok.

init_per_group(AccessContext, Config) ->
  Slave = proplists:get_value(slave, Config),
  ok = rpc:call(Slave, throttle_distributed_SUITE, start_throttle, [AccessContext]),
  start_throttle(AccessContext),

  Sleep = case AccessContext of
            %% async takes a bit to propagate the changes after the operation returns
            async_dirty -> 50;
            _ -> 0
          end,
  [{sleep, Sleep} | Config].

end_per_group(_Context, Config) ->
  application:stop(throttle),
  ok.

start_slave() ->
  Slave = 'node2@127.0.0.1',

  %% make the same binaries available to the other node
  CodePath = code:get_path(),
  PathFlag = "-pa " ++ lists:concat(lists:join(" ", CodePath)),
  {ok, _} = ct_slave:start(Slave, [{erl_flags, PathFlag}]),
  {ok, Slave}.

start_throttle(Context) ->
  application:ensure_all_started(lager),
  ok = application:set_env(throttle, driver, throttle_mnesia),
  ok = application:set_env(throttle, access_context, Context),
  {ok, _Apps} = application:ensure_all_started(throttle),
  throttle_mnesia:init(),
  ok.

test_limit(Config) ->
  Slave = proplists:get_value(slave, Config),
  Sleep = proplists:get_value(sleep, Config),
  Scope = binary_to_atom(integer_to_binary(rand:uniform(10000)), latin1),
  throttle:setup(Scope, 4, per_second),

  %% check both here and in the other node
  {ok, 3, _} = throttle:check(Scope, <<"john">>),
  timer:sleep(Sleep),
  {ok, 2, _} = rpc:call(Slave, throttle, check, [Scope, <<"john">>]),
  timer:sleep(Sleep),
  {ok, 1, _} = throttle:check(Scope, <<"john">>),
  timer:sleep(Sleep),
  {ok, 0, _} = rpc:call(Slave, throttle, check, [Scope, <<"john">>]),
  timer:sleep(Sleep),
  {limit_exceeded, 0, _} = throttle:check(Scope, <<"john">>),

  timer:sleep(1100),

  {ok, 3, _} = throttle:check(Scope, <<"john">>),

  ok.
