-module(throttle_mnesia).

-export([init/0,
         init_counters/3,
         reset_counters/1,
         update_counter/2,
         lookup_counter/2
        ]).

-record(scope_state, {scope, limit, period, previous_reset}).

%%% intialize the index table that tracks state for all the scopes
%% FIXME this will require nodes to be connected first
%% which means that maybe this step should be done manually and thus removed
%% from regular app initialization
init() ->
  %% FIXME may need to start on all nodes?
  %% rpc:multicall(Nodes, application, start, [mnesia]),
  application:start(mnesia),

  {atomic, ok} = mnesia:create_table(scope_state,
                                     [{attributes, record_info(fields, scope_state)},
                                      {ram_copies, [node() | nodes()]},
                                      {type, set}]),
  ok.

init_counters(Scope, Limit, Period) ->
  %% counter table uses the default key/value
  {atomic, ok} = mnesia:create_table(Scope, [{ram_copies, [node() | nodes()]},
                                             {type, set}]),

  AddScope = fun() ->
                 ok = mnesia:write(#scope_state{scope=Scope,
                                                limit=Limit + 1, %% add + 1 to allow up to (including) that number
                                                period=Period,
                                                previous_reset=timestamp()})
             end,

  mnesia:activity(transaction, AddScope).

reset_counters(Scope) ->
  %% clear counters
  {atomic, ok} = mnesia:clear_table(Scope),

  %% update last reset timestamp
  [State] = mnesia:dirty_read(scope_state, Scope),
  NewState = State#scope_state{previous_reset=timestamp()},
  ok = mnesia:dirty_write(scope_state, NewState).

update_counter(Scope, Key) ->
  case mnesia:dirty_read(scope_state, Scope) of
    [#scope_state{limit=Limit, period=Period, previous_reset=PreviousReset}] ->
      NextReset = throttle:interval(Period) - (timestamp() - PreviousReset),

      %% FIXME should we wrap this in an activity and let the user configure what access context to use?
      Count = mnesia:dirty_update_counter(Scope, Key, 1),
      LimitedCount = min(Limit, Count),

      {LimitedCount, Limit, NextReset};
    [] ->
      rate_not_set
  end.

lookup_counter(Scope, Key) ->
  case mnesia:dirty_read(scope_state, Scope) of
    [#scope_state{limit=Limit, period=Period, previous_reset=PreviousReset}] ->
      NextReset = throttle:interval(Period) - (timestamp() - PreviousReset),

      case mnesia:dirty_read(Scope, Key) of
        [{Scope, Key, Count}] ->
          {Count, Limit, NextReset};
        [] ->
          {0, Limit, NextReset}
      end;
    [] ->
      rate_not_set
  end.

%%% Internal functions
%% TODO if always the same move to utils or throttle.erl
timestamp() ->
  erlang:system_time(millisecond).
