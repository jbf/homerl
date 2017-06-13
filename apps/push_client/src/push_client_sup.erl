-module(push_client_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [{push_client, {push_client, start_link, []},
              permanent, 2000, worker, [push_client]}],
	{ok, {{one_for_one, 1, 5}, Procs}}.
