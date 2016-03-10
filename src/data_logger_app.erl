-module(data_logger_app).

-behaviour(application).

-export([
    start/2,
    stop/1
    ]).

start(_Type, _StartArgs) ->
    DL = case data_logger_sup:start_link() of
        {ok, Pid} -> {ok, Pid};
        Other -> {error, Other}
    end,
    ok = maybe_start_yaws(application:get_env(data_logger, start_yaws)),
    DL.

maybe_start_yaws({ok, false}) -> ok;
maybe_start_yaws({ok, true}) ->
    yaws:start_embedded(get_yaws_dir(),
                        [servername(), listen(), port()]).

get_yaws_dir() ->
    {ok, Dir} = application:get_env(data_logger, yaws_www_root),
    {ok, CWD} = file:get_cwd(),
    CWD ++ "/" ++ Dir.

servername() ->
    {ok, Ret} = application:get_env(data_logger, yaws_servername),
    {servername, Ret}.

listen() ->
    {ok, Ret} = application:get_env(data_logger, yaws_listen),
    {listen, Ret}.

port() ->
    {ok, Port} = application:get_env(data_logger, yaws_port),
    {port, Port}.

stop(_State) ->
    ok.
