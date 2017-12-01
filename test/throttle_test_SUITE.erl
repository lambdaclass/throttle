-module(throttle_test_SUITE).
-include_lib("common_test/include/ct.hrl").

-compile(export_all).

-define(ALL_TESTS, [test_limit,
                    test_peek,
                    test_scopes_not_mixed,
                    test_keys_not_mixed,
                    test_minute,
                    test_hour,
                    test_day,
                    rate_not_set]).

%% we want to repeat the same suit with the different drivers
groups() ->
  [{throttle_ets, [], ?ALL_TESTS}].

all() ->
  [{group, throttle_ets}].

init_per_group(Driver, Config) ->
  ok = application:set_env(throttle, driver, Driver),
  {ok, _Apps} = application:ensure_all_started(throttle),
  Config.

end_per_group(_Driver, _Config) ->
  ok = application:stop(throttle),
  ok = application:unload(throttle).

test_limit(_Config) ->
  throttle:setup(test_rate, 3, per_second),

  {ok, 2, _} = throttle:check(test_rate, <<"john">>),
  {ok, 1, _} = throttle:check(test_rate, <<"john">>),
  {ok, 0, _} = throttle:check(test_rate, <<"john">>),
  {limit_exceeded, 0, _} = throttle:check(test_rate, <<"john">>),

  timer:sleep(1001),

  {ok, 2, _} = throttle:check(test_rate, <<"john">>),

  ok.

test_peek(_Config) ->
  throttle:setup(test_rate2, 2, per_second),

  {ok, 2, _} = throttle:peek(test_rate2, <<"john">>),
  {ok, 2, _} = throttle:peek(test_rate2, <<"john">>),
  {ok, 2, _} = throttle:peek(test_rate2, <<"john">>),

  ok.

test_scopes_not_mixed(_Config) ->
  throttle:setup(test_rate3, 3, per_second),
  throttle:setup(test_rate4, 3, per_second),

  {ok, 2, _} = throttle:check(test_rate3, <<"john">>),
  {ok, 1, _} = throttle:check(test_rate3, <<"john">>),
  {ok, 0, _} = throttle:check(test_rate3, <<"john">>),
  {limit_exceeded, 0, _} = throttle:check(test_rate3, <<"john">>),

  {ok, 2, _} = throttle:check(test_rate4, <<"john">>),

  ok.

test_keys_not_mixed(_Config) ->
  throttle:setup(test_rate5, 2, per_second),

  {ok, 1, _} = throttle:check(test_rate5, <<"john">>),
  {ok, 0, _} = throttle:check(test_rate5, <<"john">>),
  {limit_exceeded, 0, _} = throttle:check(test_rate5, <<"john">>),

  {ok, 1, _} = throttle:check(test_rate5, <<"mary">>),

  ok.

test_minute(_Config) ->
  throttle:setup(test_rate6, 3, per_minute),

  {ok, 2, TimeToReset} = throttle:check(test_rate6, <<"john">>),

  Diff = 1000 * 60 - TimeToReset,
  true = Diff < 1000,

  ok.

test_hour(_Config) ->
  throttle:setup(test_rate7, 3, per_hour),

  {ok, 2, TimeToReset} = throttle:check(test_rate7, <<"john">>),

  Diff = 1000 * 60 * 60 - TimeToReset,
  true = Diff < 1000,

  ok.

test_day(_Config) ->
  throttle:setup(test_rate8, 3, per_day),

  {ok, 2, TimeToReset} = throttle:check(test_rate8, <<"john">>),

  Diff = 1000 * 60 * 60 * 24 - TimeToReset,
  true = Diff < 1000,

  ok.

rate_not_set(_Config) ->
  rate_not_set = throttle:check(didnt_set, <<"john">>),

  ok.
