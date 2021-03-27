-module(throttle_mnesia).

-behavior(throttle_driver).

-export([
         init/0,
         init_counters/3,
         reset_counters/1,
         update_counter/2,
         lookup_counter/2
        ]).

-record(scope_state, {scope, limit, period, previous_reset, access_context}).

init() ->
  %% intially this will be called by the sup and every node will get its local
  %% version of the tables. After connecting, call this manually so a single
  %% copy of the table is shared across nodes.

  AllNodes = [node() | nodes()],
  OtherNodes = nodes(),

  case application:start(mnesia) of
    ok ->
      %% first time, create state table
      {atomic, ok} = mnesia:create_table(scope_state,
                                         [{attributes, record_info(fields, scope_state)},
                                          {ram_copies, AllNodes},
                                          {type, set}]);
    {error, {already_started, mnesia}} ->
      %% not the first time, add new nodes
      rpc:multicall(OtherNodes, application, stop, [mnesia]),
      rpc:multicall(OtherNodes, application, start, [mnesia]),
      mnesia:change_config(extra_db_nodes, AllNodes)
  end,
  ok.

init_counters(Scope, Limit, Period) ->
  %% counter table uses the default key/value
  {atomic, ok} = mnesia:create_table(Scope, [{ram_copies, [node() | nodes()]},
                                             {type, set}]),

  AccessContext = application:get_env(throttle, access_context, async_dirty),
  AddScope = fun() ->
                 ok = mnesia:write(#scope_state{scope=Scope,
                                                limit=Limit + 1, %% add + 1 to allow up to (including) that number
                                                period=Period,
                                                previous_reset=throttle_time:now(),
                                                access_context=AccessContext})
             end,

  mnesia:activity(transaction, AddScope).

reset_counters(Scope) ->
  {atomic, ok} = mnesia:clear_table(Scope),

  %% update last reset timestamp
  [State] = mnesia:dirty_read(scope_state, Scope),
  NewState = State#scope_state{previous_reset=throttle_time:now()},
  ok = mnesia:dirty_write(scope_state, NewState).

update_counter(Scope, Key) ->
  case mnesia:dirty_read(scope_state, Scope) of
    [#scope_state{limit=Limit,
                  period=Period,
                  previous_reset=PreviousReset,
                  access_context=AccessContext}] ->
      NextReset = throttle_time:next_reset(Period, PreviousReset),

      UpdateCounter = fun() ->
                          mnesia:dirty_update_counter(Scope, Key, 1)
                      end,

      Count = mnesia:activity(AccessContext, UpdateCounter),
      LimitedCount = min(Limit, Count),

      {LimitedCount, Limit, NextReset};
    [] ->
      rate_not_set
  end.

lookup_counter(Scope, Key) ->
  case mnesia:dirty_read(scope_state, Scope) of
    [#scope_state{limit=Limit, period=Period, previous_reset=PreviousReset}] ->
      NextReset = throttle_time:next_reset(Period, PreviousReset),

      case mnesia:dirty_read(Scope, Key) of
        [{Scope, Key, Count}] ->
          {Count, Limit, NextReset};
        [] ->
          {0, Limit, NextReset}
      end;
    [] ->
      rate_not_set
  end.
