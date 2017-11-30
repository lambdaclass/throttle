-module(throttle_ets_delete).

-export([init/0,
         init_counters/3,
         reset_counters/1,
         update_counter/2,
         lookup_counter/2
        ]).

-define(STATE_TABLE, throttle_state_table).

%%% intialize the index table that tracks state for all the scopes
init() ->
  ets:new(?STATE_TABLE, [set, named_table, public]),
  ok.

init_counters(Scope, Limit, Period) ->
  TableId = ets:new(scope_counters, [set, public]),
  %% add + 1 to allow up to (including) that number
  ets:insert(?STATE_TABLE, {Scope, TableId, Limit + 1, Period, timestamp()}).

reset_counters(Scope) ->
  [{Scope, TableId, Limit, Period, _PreviousReset}] = ets:lookup(?STATE_TABLE, Scope),
  true = ets:delete_all_objects(TableId),
  true = ets:insert(?STATE_TABLE, {Scope, TableId, Limit, Period, timestamp()}),
  ok.

%% TODO make sure interval makes sense here or can be factored out to the server
update_counter(Scope, Key) ->
  case ets:lookup(?STATE_TABLE, Scope) of
    [{Scope, TableId, Limit, Period, PreviousReset}] ->
      NextReset = throttle:interval(Period) - (timestamp() - PreviousReset),

      %% add 1 to counter in position 2, if it's less or equal than Limit, default counter to 0
      Count = ets:update_counter(TableId, Key, {2, 1, Limit, Limit}, {Key, 0}),

      {Count, Limit, NextReset};
    [] ->
      rate_not_set
  end.

lookup_counter(Scope, Key) ->
  case ets:lookup(?STATE_TABLE, Scope) of
    [{Scope, TableId, Limit, Period, PreviousReset}] ->
      NextReset = throttle:interval(Period) - (timestamp() - PreviousReset),

      case ets:lookup(TableId, Key) of
        [{Key, Count}] ->
          {Count, Limit, NextReset};
        [] ->
          {0, Limit, NextReset}
      end;
    [] ->
      rate_not_set
  end.

%%% Internal functions
timestamp() ->
  erlang:system_time(millisecond).
