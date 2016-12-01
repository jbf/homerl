-module(device_directory_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Procs = [{device_directory, {device_directory, start_link, []},
              permanent, 2000, worker, [device_directory]}],
  {ok, {{one_for_one, 1, 5}, Procs}}.
