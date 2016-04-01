-module(data_logger_app).

-behaviour(application).

-export([
    start/2,
    stop/1
    ]).

start(_Type, _StartArgs) ->
    case data_logger_sup:start_link() of
        {ok, Pid} -> Yaws = maybe_start_yaws(application:get_env(data_logger, start_yaws)),
            {ok, Pid, Yaws};
        Other -> {error, Other}
    end.

maybe_start_yaws({ok, false}) -> not_started;
maybe_start_yaws({ok, true}) ->
    ok = yaws:start_embedded(get_yaws_dir(),
                        [servername(), listen(), port()],
                        [],
                        get_id()),
    yaws_started.

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

get_id() ->
    {ok, Id} = application:get_env(data_logger, yaws_id),
    Id.

stop(_State) ->
    ok.
