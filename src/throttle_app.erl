-module(throttle_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  %% other single node options: throttle_ets_drop, throttl_ets_single
  Driver = application:get_env(throttle, driver, throttle_ets_delete),
  Driver:init(),

  case application:get_env(throttle, rates) of
    {ok, Rates} ->
      lists:foreach(fun({Scope, Limit, Period}) ->
                        throttle:setup(Scope, Limit, Period)
                    end, Rates);
    _ ->
      ok
  end,
  throttle_sup:start_link().

stop(_State) ->
  ok.
