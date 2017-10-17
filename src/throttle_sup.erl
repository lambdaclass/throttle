%%%-------------------------------------------------------------------
%% @doc throttle top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(throttle_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% API functions

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% Supervisor callbacks

init([]) ->
  throttle:init_state_index(),

  {ok, { #{ strategy => simple_one_for_one, intensity => 5, period => 1 },
         [#{
             id => throttle,
             start => {throttle, start_link, []},
             restart => transient,
             shutdown => 5000,
             type => worker,
             modules => [throttle]
           }]
       }}.
