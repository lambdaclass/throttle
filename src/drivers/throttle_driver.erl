-module(throttle_driver).

-export([setup/0,
         initialize/3,
         reset/1,
         update/2,
         lookup/2]).

%% FIXME rename all callbacks to align with interface

%% TODO doc
-callback init() -> ok.

%% TODO doc
-callback init_counters(throttle:scope(), throttle:rate_limit(), throttle_time:interval()) -> ok.

%% TODO doc
-callback reset_counters(throttle:scope()) -> ok.

%% TODO doc
%% FIXME this should return PreviousReset and Period instead of forcing every driver to repeat that calculation
-callback update_counter(throttle:scope(), Key :: term()) -> {Count :: pos_integer(), throttle:rate_limit(), NextReset :: integer()} | rate_not_set.

-callback lookup_counter(throttle:scope(), Key :: term()) -> {Count :: pos_integer(), throttle:rate_limit(), NextReset :: integer()} | rate_not_set.

setup() ->
    Module = callback_module(),
    Module:init().

initialize(Scope, Limit, Period) ->
    Module = callback_module(),
    Module:init_counters(Scope, Limit, Period).

reset(Scope) ->
    Module = callback_module(),
    Module:reset_counters(Scope).

update(Scope, Key) ->
    Module = callback_module(),
    Module:update_counter(Scope, Key).

lookup(Scope, Key) ->
    Module = callback_module(),
    Module:lookup_counter(Scope, Key).


%%% Internal
callback_module() ->
    application:get_env(throttle, driver, throttle_ets).
