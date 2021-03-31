-module(throttle).

-behaviour(gen_server).

-export([setup/3,
         check/2,
         peek/2,

         start_link/3,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-type scope() :: atom().
-type rate_limit() :: pos_integer().

-export_type([scope/0, rate_limit/0]).

%% API functions

%%% setup throttling for a specific scope
setup(Scope, RateLimit, RatePeriod) ->
  {ok, _Pid} = supervisor:start_child(throttle_sup, [Scope, RateLimit, RatePeriod]),
  ok.

check(Scope, Key) ->
  Result = throttle_driver:update(Scope, Key),
  count_result(Result).

peek(Scope, Key) ->
  Result = throttle_driver:lookup(Scope, Key),
  count_result(Result).

%% Gen server callbacks
start_link(Scope, Limit, Period) ->
  gen_server:start_link(?MODULE, {Scope, Limit, Period}, []).

init({Scope, Limit, Period} = State) ->
  NextReset = throttle_time:schedule_reset(Period),
  throttle_driver:initialize(Scope, Limit, NextReset),
  {ok, _} = timer:send_interval(throttle_time:interval(Period), reset_counters),
  {ok, State}.

handle_info(reset_counters, {Scope, _Limit, Period} = State) ->
  NextReset = throttle_time:schedule_reset(Period),
  throttle_driver:reset(Scope, NextReset),
  {noreply, State}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

%%% Internal functions
count_result({Count, Limit, NextReset}) when Count == Limit ->
  LeftToReset = throttle_time:left_til(NextReset),
  {limit_exceeded, 0, LeftToReset};
count_result({Count, Limit, NextReset}) ->
  LeftToReset = throttle_time:left_til(NextReset),
  {ok, Limit - Count - 1, LeftToReset};
count_result(rate_not_set) ->
  rate_not_set.
