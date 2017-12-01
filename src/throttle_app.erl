-module(throttle_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  %% start supervisor first so its available when calling throttle:setup
  {ok, Pid} = throttle_sup:start_link(),

  case application:get_env(throttle, rates) of
    {ok, Rates} ->
      lists:foreach(fun({Scope, Limit, Period}) ->
                        throttle:setup(Scope, Limit, Period)
                    end, Rates);
    _ ->
      ok
  end,
  {ok, Pid}.

stop(_State) ->
  ok.
