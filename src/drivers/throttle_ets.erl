-module(throttle_ets).

-behavior(throttle_driver).

-export([setup/0,
         initialize/3,
         reset/1,
         update/2,
         lookup/2]).

-define(STATE_TABLE, throttle_state_table).

%%% intialize the index table that tracks state for all the scopes
setup() ->
  ets:new(?STATE_TABLE, [set, named_table, public]),
  ok.

initialize(Scope, Limit, Period) ->
  TableId = ets:new(scope_counters, [set, public]),
  %% add + 1 to allow up to (including) that number
  ets:insert(?STATE_TABLE, {Scope, TableId, Limit + 1, Period, throttle_time:now()}).

reset(Scope) ->
  [{Scope, TableId, Limit, Period, _PreviousReset}] = ets:lookup(?STATE_TABLE, Scope),
  true = ets:delete_all_objects(TableId),
  true = ets:insert(?STATE_TABLE, {Scope, TableId, Limit, Period, throttle_time:now()}),
  ok.

update(Scope, Key) ->
  case ets:lookup(?STATE_TABLE, Scope) of
    [{Scope, TableId, Limit, Period, PreviousReset}] ->
      NextReset = throttle_time:next_reset(Period, PreviousReset),

      %% add 1 to counter in position 2, if it's less or equal than Limit, default counter to 0
      Count = ets:update_counter(TableId, Key, {2, 1, Limit, Limit}, {Key, 0}),

      {Count, Limit, NextReset};
    [] ->
      rate_not_set
  end.

lookup(Scope, Key) ->
  case ets:lookup(?STATE_TABLE, Scope) of
    [{Scope, TableId, Limit, Period, PreviousReset}] ->
      NextReset = throttle_time:next_reset(Period, PreviousReset),

      case ets:lookup(TableId, Key) of
        [{Key, Count}] ->
          {Count, Limit, NextReset};
        [] ->
          {0, Limit, NextReset}
      end;
    [] ->
      rate_not_set
  end.
