-module(throttle_time).

-export([now/0,
         interval/1,
         next_reset/2]).

-type interval() :: per_day | per_hour | per_minute | per_second | pos_integer().
-export_type([interval/0]).

now() ->
  erlang:system_time(millisecond).

interval(per_day) ->
  1000 * 60 * 60 * 24;
interval(per_hour) ->
  1000 * 60 * 60;
interval(per_minute) ->
  1000 * 60;
interval(per_second) ->
  1000;
interval(CustomMs) when is_integer(CustomMs) ->
  CustomMs.

next_reset(Period, Previous) ->
  interval(Period) - (throttle_time:now() - Previous).
