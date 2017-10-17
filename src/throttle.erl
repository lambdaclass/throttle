-module(throttle).
-behaviour(gen_server).

-export([init_state_index/0,
         setup/3,
         check/2,
         peek/2,

         start_link/3,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2
        ]).

-define(STATE_TABLE, throttle_state_table).

%% API functions

%%% intialize the index table that tracks state for all the scopes
init_state_index() ->
  ets:new(?STATE_TABLE, [set, named_table, public]),
  ok.

%%% setup throttling for a specific scope
setup(Scope, RateLimit, RatePeriod) ->
  %% TODO start child in supervisor
  {ok, _Pid} = supervisor:start_child(throttle_sup, [Scope, RateLimit, RatePeriod]),
  ok.

check(Scope, Key) ->
  count_result(update_counter(Scope, Key)).

peek(Scope, Key) ->
  count_result(lookup_counter(Scope, Key)).

%% Gen server callbacks
start_link(Scope, Limit, Period) ->
  gen_server:start_link(?MODULE, {Scope, Limit, Period}, []).

init({Scope, Limit, Period} = State) ->
  init_counters(Scope, Limit, Period),
  {ok, State}.

handle_info(reset_counters, {Scope, _Limit, _Period} = State) ->
  reset_counters(Scope),
  {noreply, State}.

handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

%%% Internal functions
timestamp() ->
  erlang:system_time(millisecond).

interval(per_day) ->
  1000 * 60 * 60 * 24;
interval(per_hour) ->
  1000 * 60 * 60;
interval(per_minute) ->
  1000 * 60;
interval(per_second) ->
  1000.

init_counters(Scope, Limit, Period) ->
  TableId = ets:new(scope_counters, [set, public]),

  ets:insert(?STATE_TABLE, {Scope, TableId, Limit, Period, timestamp()}),
  {ok, _} = timer:send_interval(interval(Period), reset_counters),
  ok.

reset_counters(Scope) ->
  [{Scope, TableId, Limit, Period, _PreviousReset}] = ets:lookup(?STATE_TABLE, Scope),
  true = ets:delete_all_objects(TableId),
  true = ets:insert(?STATE_TABLE, {Scope, TableId, Limit, Period, timestamp()}),
  ok.

update_counter(Scope, Key) ->
  [{Scope, TableId, Limit, Period, PreviousReset}] = ets:lookup(?STATE_TABLE, Scope),
  NextReset = interval(Period) - (timestamp() - PreviousReset),

  %% add 1 to counter in position 2, if it's less or equal than Limit, default counter to 0
  Count = ets:update_counter(TableId, Key, {2, 1, Limit, Limit}, {Key, 0}),

  {Count, Limit, NextReset}.

lookup_counter(Scope, Key) ->
  [{Scope, TableId, Limit, Period, PreviousReset}] = ets:lookup(?STATE_TABLE, Scope),
  NextReset = interval(Period) - (timestamp() - PreviousReset),
  [{Key, Count}] = ets:lookup(TableId, Key),
  {Count, Limit, NextReset}.

count_result({Count, Count, NextReset}) ->
    {limit_exceeded, NextReset};
count_result({Count, Limit, NextReset}) ->
    {ok, Limit - Count, NextReset}.
