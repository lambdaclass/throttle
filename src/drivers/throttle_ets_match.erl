-module(throttle_ets_match).

-export([init/0,
         init_counters/3,
         reset_counters/1,
         update_counter/2,
         lookup_counter/2
        ]).

-define(STATE_TABLE, throttle_state_table).
-define(COUNTER_TABLE, throttle_counter_table).

%%% intialize the index table that tracks state for all the scopes
init() ->
  ets:new(?STATE_TABLE, [set, named_table, public]),
  ets:new(?COUNTER_TABLE, [set, named_table, public]),
  ok.

init_counters(Scope, Limit, Period) ->
  ets:insert(?STATE_TABLE, {Scope, Limit + 1, Period, timestamp()}).

reset_counters(Scope) ->
  [{Scope, Limit, Period, _PreviousReset}] = ets:lookup(?STATE_TABLE, Scope),
  %% delete all entries in the counter table belonging to this scope
  Pattern = {{Scope, '_'}, '_'},
  true = ets:match_delete(?COUNTER_TABLE, Pattern),
  true = ets:insert(?STATE_TABLE, {Scope, Limit, Period, timestamp()}),
  ok.

%% TODO make sure interval makes sense here or can be factored out to the server
update_counter(Scope, Key) ->
  FullKey = {Scope, Key},
  case ets:lookup(?STATE_TABLE, Scope) of
    [{Scope, Limit, Period, PreviousReset}] ->
      NextReset = throttle:interval(Period) - (timestamp() - PreviousReset),

      %% add 1 to counter in position 2, if it's less or equal than Limit, default counter to 0
      Count = ets:update_counter(?COUNTER_TABLE, FullKey, {2, 1, Limit, Limit}, {FullKey, 0}),

      {Count, Limit, NextReset};
    [] ->
      rate_not_set
  end.

lookup_counter(Scope, Key) ->
  FullKey = {Scope, Key},
  case ets:lookup(?STATE_TABLE, Scope) of
    [{Scope, Limit, Period, PreviousReset}] ->
      NextReset = throttle:interval(Period) - (timestamp() - PreviousReset),

      case ets:lookup(?COUNTER_TABLE, FullKey) of
        [{FullKey, Count}] ->
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
