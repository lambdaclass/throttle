# throttle

An OTP application

## Build

    $ rebar3 compile

## Usage

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
