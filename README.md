# throttle

An OTP application to implement throttling/rate limiting of resources.

## Build

    $ rebar3 compile

## Usage

The application allows to limit different resources (scopes) at different rates.

*  `throttle:setup(Scope, RateLimit, RatePeriod)`: setup a rate limit
   for a given `Scope`, allowing at most `RateLimit` requests per
   `RatePeriod`. Allowed rate periods are `per_second`, `per_minute`,
   `per_hour` and `per_day`.

   Rates can also be set via application environment instead of
   calling `setup`:

   ```erlang
   {throttle, [{rates, [{my_global_scope, 10, per_second}
                        {my_expensive_endpoint, 2, per_minute}]}]}
   ```

* `throttle:check(Scope, Key)`: attempt to request `Scope` with a
  given `Key` (e.g. user token, IP). The result will be `{ok,
  RemainingAttempts, TimeToReset}` if there are attempts left or
  `{limit_exceeded, 0, TimeToReset}` if there aren't.

* `throttle:peek(Scope, Key)`: returns the same result as `check`
  without increasing the requests count.

## Examples

### Shell
``` erlang
1> application:ensure_all_started(throttle).
{ok,[throttle]}
2> throttle:setup(my_api_endpoint, 3, per_minute).
ok
3> throttle:check(my_api_endpoint, my_token_or_ip).
{ok,2,30362}
4> throttle:check(my_api_endpoint, my_token_or_ip).
{ok,1,29114}
5> throttle:check(my_api_endpoint, my_token_or_ip).
{ok,0,27978}
6> throttle:check(my_api_endpoint, my_token_or_ip).
{limit_exceeded,0,26722}
```

### Cowboy 2.0 limit by IP

Middleware module:

``` erlang
-module(throttling_middleware).

-behavior(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
  {{IP, _}, Req2} = cowboy_req:peer(Req),

  case throttle:check(my_api_rate, IP) of
    {limit_exceeded, _, _} ->
      lager:warning("IP ~p exceeded api limit", [IP]),
      Req3 = cowboy_req:reply(429, Req2),
      {stop, Req3};
    _ ->
      {ok, Req2, Env}
  end.
```

Using it:

``` erlang
cowboy:start_clear(my_http_listener, [{port, 8080}], #{
		env => #{dispatch => Dispatch},
		middlewares => [cowboy_router, throttling_middleware, cowboy_handler]
	}),
```

### Cowboy 2.0 limit by Authorization header

``` erlang
-module(throttling_middleware).

-behavior(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
  Authorization = cowboy_req:header(<<"authorization">>, Req),

  case throttle:check(my_api_rate, Authorization) of
    {limit_exceeded, _, _} ->
      lager:warning("Auth ~p exceeded api limit", [Authorization]),
      Req3 = cowboy_req:reply(429, Req),
      {stop, Req2};
    _ ->
      {ok, Req, Env}
  end.
```

Note that assumes all requests have an authorization header. A more
realistic approach would be to fallback to an IP limit when
Authorization is not present.

### Cowboy 1.0 limit by IP

Middleware module:

``` erlang
-module(throttling_middleware).

-behavior(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
  {{IP, _}, Req2} = cowboy_req:peer(Req),

  case throttle:check(my_api_rate, IP) of
    {limit_exceeded, _, _} ->
      lager:warning("IP ~p exceeded api limit", [IP]),
      {error, 429, Req2};
    _ ->
      {ok, Req2, Env}
  end.
```

Using it:

``` erlang
cowboy:start_http(my_http_listener, 100, [{port, 8080}],
                    [{env, [{dispatch, Dispatch}]},
                     {middlewares, [cowboy_router, throttling_middleware, cowboy_handler]}]
                   ),
```

A more detailed example, choosing the rate based on the path, can be found [here](https://github.com/lambdaclass/holiday_ping/blob/26a3d83faaad6977c936a40fe273cd45954d9259/src/throttling_middleware.erl).
