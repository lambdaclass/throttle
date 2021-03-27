-module(throttle_driver).

-export([setup/0,
         initialize/3,
         reset/1,
         update/2,
         lookup/2]).

%% TODO doc
-callback init() -> ok.

%% TODO doc
-callback init_counters(throttle:scope(), throttle:rate_limit(), throttle_time:interval()) -> ok.

%% TODO doc
-callback reset_counters(throttle:scope()) -> ok.

%% TODO doc
%% FIXME this should return PreviousReset and Period instead of forcing every driver to repeat that calculation
-callback update_counter(throttle:scope(), Key :: term()) ->
    {Count :: pos_integer(), throttle:rate_limit(), NextReset :: integer()} | rate_not_set.

-callback lookup_counter(throttle:scope(), Key :: term()) ->
    {Count :: pos_integer(), throttle:rate_limit(), NextReset :: integer()} | rate_not_set.

setup() ->
    Module = callback_module(),
    Module:setup().

initialize(Scope, Limit, Period) ->
    Module = callback_module(),
    Module:initialize(Scope, Limit, Period).

reset(Scope) ->
    Module = callback_module(),
    Module:reset(Scope).

update(Scope, Key) ->
    Module = callback_module(),
    Module:update(Scope, Key).

lookup(Scope, Key) ->
    Module = callback_module(),
    Module:lookup(Scope, Key).


%%% Internal
callback_module() ->
    application:get_env(throttle, driver, throttle_ets).
