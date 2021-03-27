%%
%% This behavior defines the interface that throttle uses to interact with the
%% different stores that track scope/key access counts.
%%
-module(throttle_driver).

-export([setup/0,
         initialize/3,
         reset/1,
         update/2,
         lookup/2]).

%% Performs any global (non scope-specific) setup required by the driver.
-callback setup() -> ok.

%% Performs scope-specific initialization of a scope.
-callback initialize(throttle:scope(), throttle:rate_limit(), throttle_time:interval()) -> ok.

%% Resets all the key counters for the scope back to zero.
-callback reset(throttle:scope()) -> ok.

%% FIXME this should return PreviousReset and Period instead of forcing every driver to repeat that calculation
%% it would probably be more appropriate to store the scope metadata elsewhere

%% Increase the access count for the scope/key and return its current value.
-callback update(throttle:scope(), Key :: term()) ->
    {Count :: pos_integer(), throttle:rate_limit(), NextReset :: integer()} | rate_not_set.

%% Retrieve the current access count for the scope/key without increasing it.
-callback lookup(throttle:scope(), Key :: term()) ->
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
