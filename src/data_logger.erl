-module(data_logger).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/1]).

%% gen_server callbacks
-export([terminate/2, init/1, handle_info/2]).
-export([handle_call/3, handle_cast/2]).
-export([code_change/3]).

-record(state, {port, err_log, raw_log, outfile}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

stop(Ref) ->
    gen_server:cast(Ref, stop).

init(_Args) ->
    {ok, Binary} = application:get_env(?MODULE, binary),
    Executable = code:priv_dir(?MODULE) ++ "/" ++ Binary,
    {ok, Dir} = application:get_env(?MODULE, data_dir),
    {ok, Errlog} = file:open(Dir ++ "/err.log", [write]),
    {ok, Log} = file:open(Dir ++ "/raw.txt", [append]),
    {ok, Log2} = file:open(Dir ++ "/out.txt", [append]),
    Port = open_port({spawn_executable, Executable}, [{packet, 2}]),
    case erlang:port_info(Port) of
        undefined -> {stop, can_not_open_port};
        _ ->
            State = #state{port = Port,
                           err_log = Errlog,
                           raw_log = Log,
                           outfile = Log2},
            {ok, State}
    end.

terminate({port_terminated, _Reason}, _State) ->
    ok;
terminate(_Reason, _State = #state{port=Port,
                                   err_log = Errlog,
                                   raw_log = Raw,
                                   outfile = Out}) ->
    % loop here until port is closed
    Port ! {self(), close},
    term(1, Port),
    file:close(Out),
    file:close(Raw),
    file:close(Errlog),
    ok.

term(11, _P) -> ok; % Give up
term(Count, Port) ->
    receive
        {Port, closed} -> ok;
        Msg ->
            io:format(standard_error, "Message while waiting for closing port: ~p~n", [Msg]),
            term(Count + 1, Port)
    after
        10000 ->
            io:format(standard_error, "Gave up waiting for port: ~p~n", [Port]),
            ok
    end.

handle_info({Port, {data, Data}}, State = #state{port=Port,
                                                 outfile=Out,
                                                 raw_log=Raw}) ->
    Decoded = decode(Data),
    io:format(Raw, "~p~n", [Decoded]),
    {DeviceId, _DataType, _Value, _TimeStamp} = Decoded,
    case sensor_directory:blacklisted_sensor(DeviceId) of
        true -> {noreply, State};
        _    ->
            pretty_print(Out, Decoded),
            {noreply, State}
    end;
handle_info({'EXIT', Port, Reason}, State = #state{port=Port}) ->
    {stop, {port_terminated, Reason}, State}.

decode(Data) ->
    {ok, Tokens, _} = erl_scan:string(Data),
    {ok, Ret} = erl_parse:parse_term(Tokens),
    Ret.

pretty_print(To, {DeviceId, DataType, Value, TimeStamp}) ->
    {ok, {sensor, DeviceId, _}} = sensor_directory:get_sensor(DeviceId),
    Time = timestamp_to_datestring(TimeStamp),
    Unit = case DataType of
        h -> "%";
        t -> "C";
        _ -> ""
    end,
    DataType2 = case DataType of
        h -> humidity;
        t -> temperature;
        X -> X
    end,
    io:format(To, "~s device=~p, value=~p~s, type=~p~n",
              [Time, DeviceId, Value, Unit, DataType2]),
    ok.

timestamp_to_datestring(TimeStamp) ->
    SecToEpoch = 62167219200,
    TotalSec = SecToEpoch + TimeStamp,
    {{Year, Month, Day}, {Hour, Minute, Second}} =
      calendar:gregorian_seconds_to_datetime(TotalSec),
    io_lib:format("~B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B.~2.10.0B",
      [Year, Month, Day, Hour, Minute, Second]).

handle_cast('###crash_me', State) ->
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_call('###crash_me', _From, State) ->
    {noreply, State}.

code_change(_Old, State, _Extra) ->
    {ok, State}.
