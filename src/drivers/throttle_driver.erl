%%
%% Interface to interact with the different stores that can track scope/key
%% access counts.
%%
-module(throttle_driver).

-export([setup/0,
         initialize/3,
         reset/2,
         update/3,
         lookup/2]).

%% Performs any global (non scope-specific) setup required by the driver.
-callback setup() -> ok.

%% Performs scope-specific initialization of a scope.
-callback initialize(throttle:scope(), throttle:rate_limit(), NextReset :: integer()) -> ok.

%% Resets all the key counters of the scope back to zero.
-callback reset(throttle:scope(), NextReset :: integer()) -> ok.

%% Increase the access count for the scope/key and return its current value.
-callback update(throttle:scope(), Key :: term(), Value :: pos_integer()) ->
    {Count :: pos_integer(), throttle:rate_limit(), NextReset :: integer()} | rate_not_set.

%% Retrieve the current access count for the scope/key without increasing it.
-callback lookup(throttle:scope(), Key :: term()) ->
    {Count :: pos_integer(), throttle:rate_limit(), NextReset :: integer()} | rate_not_set.

setup() ->
    Module = callback_module(),
    Module:setup().

initialize(Scope, Limit, NextReset) ->
    Module = callback_module(),
    Module:initialize(Scope, Limit, NextReset).

reset(Scope, NextReset) ->
    Module = callback_module(),
    Module:reset(Scope, NextReset).

update(Scope, Key, Value) ->
    Module = callback_module(),
    Module:update(Scope, Key, Value).

lookup(Scope, Key) ->
    Module = callback_module(),
    Module:lookup(Scope, Key).


%%% Internal
callback_module() ->
    application:get_env(throttle, driver, throttle_ets).
