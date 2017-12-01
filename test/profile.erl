-module(profile).

-export([stress/4,
         profile/1]).

%% TODO shorter arity with sane defaults

stress(Driver, Scopes, Limit, Processes) ->
  init(Driver, Scopes, Limit, Processes),
  timer:sleep(500),

  %% start profiler
  {ok, _} = eprof:profile(fun() ->
                              throttle:check(1, self())
                          end),

  eprof:stop_profiling(),
  eprof:analyze().

%% test the standalone reset_counters function
%%  which is what changes between ets implementation
profile(Driver) ->
  Scope = profile_test,
  Driver:init(),
  Driver:init_counters(Scope, 1000, per_second),

  %% setup some counters
  lists:foreach(fun(N) ->
                    Driver:update_counter(Scope, N)
                end, lists:seq(0, 1000)),

  %% time reset counters
  Result = timer:tc(Driver, reset_counters, [Scope]),

  %% cleanup ets
  ets:delete(throttle_state_table),
  case Driver of
    throttle_ets_match -> ets:delete(throttle_counter_table);
    _ -> ok
  end,
  Result.

%% internal
init(Driver, Scopes, Limit, Processes) ->
  application:set_env(throttle, driver, Driver),
  application:set_env(throttle, rates, [{N, Limit, per_second} || N <- lists:seq(0, Scopes)]),
  {ok, _Started} = application:ensure_all_started(throttle),
  spawn_processes(Scopes, Processes).

spawn_processes(Scopes, Processes) ->
  lists:foreach(fun (N) ->
                    Scope = N rem Scopes,
                    %% spawn process that constantly queries throttle
                    spawn(fun() -> loop(Scope) end)
                end, lists:seq(0, Processes)).

loop(Scope) ->
  case throttle:check(Scope, self()) of
    {ok, _, _} ->
      dumb_operation(),
      loop(Scope);
    {limit_exceeded, _, _} ->
      loop(Scope)
  end.

dumb_operation() ->
  %% does this make sense?
  1 + 1.
