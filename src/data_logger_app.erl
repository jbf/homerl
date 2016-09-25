-module(data_logger_app).

-behaviour(application).

-export([
    start/2,
    stop/1
    ]).

start(_Type, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        % {HostMatch, list({PathMatch, Handler, Opts})}
        {'_', [
               {"/", sensor_index_handler, []},
               {"/sensor", sensor_handler, []}
              ]}
    ]),
    {ok, _} = cowboy:start_http(http_listener,
        100,
        [{port, cowboy_port()}],
        [{env, [{dispatch, Dispatch}]}]),
    data_logger_sup:start_link().

cowboy_port() ->
    {ok, Port} = application:get_env(data_logger, cowboy_port),
    Port.

stop(_State) ->
    ok.
