-module(throttle).

-behaviour(gen_server).

-export([setup/3,
         check/2,
         peek/2,

         start_link/3,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2
        ]).

-type scope() :: atom().
-type rate_limit() :: pos_integer().

-export_type([scope/0, rate_limit/0]).

%% API functions

%%% setup throttling for a specific scope
setup(Scope, RateLimit, RatePeriod) ->
  {ok, _Pid} = supervisor:start_child(throttle_sup, [Scope, RateLimit, RatePeriod]),
  ok.

check(Scope, Key) ->
  Result = driver_call(update_counter, [Scope, Key]),
  count_result(Result).

peek(Scope, Key) ->
  Result = driver_call(lookup_counter, [Scope, Key]),
  count_result(Result).

%% Gen server callbacks
start_link(Scope, Limit, Period) ->
  gen_server:start_link(?MODULE, {Scope, Limit, Period}, []).

init({Scope, Limit, Period} = State) ->
  driver_call(init_counters, [Scope, Limit, Period]),
  {ok, _} = timer:send_interval(throttle_time:interval(Period), reset_counters),
  {ok, State}.


handle_info(reset_counters, {Scope, _Limit, _Period} = State) ->
  driver_call(reset_counters, [Scope]),
  {noreply, State}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

%%% Internal functions
driver_call(F, A) ->
  Driver = application:get_env(throttle, driver, throttle_ets),
  apply(Driver, F, A).

count_result({Count, Limit, NextReset}) when Count == Limit ->
    {limit_exceeded, 0, NextReset};
count_result({Count, Limit, NextReset}) ->
    {ok, Limit - Count - 1, NextReset};
count_result(rate_not_set) ->
  rate_not_set.
