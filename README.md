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

### Example
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
