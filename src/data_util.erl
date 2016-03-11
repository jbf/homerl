-module(data_util).

-export([ decode/1, format/1, pretty_print/2, timestamp_to_datestring/1 ]).

decode(Data) ->
    {ok, Tokens, _} = erl_scan:string(Data),
    {ok, Ret} = erl_parse:parse_term(Tokens),
    Ret.

format({DeviceId, DataType, Value, TimeStamp}) ->
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
    [Time, DeviceId, Value, Unit, DataType2].

pretty_print(To, ArgsList) ->
    io:format(To, "~s device=~p, value=~p~s, type=~p~n", ArgsList),
    ok.

timestamp_to_datestring(TimeStamp) ->
    SecToEpoch = 62167219200,
    TotalSec = SecToEpoch + TimeStamp,
    {{Year, Month, Day}, {Hour, Minute, Second}} =
      calendar:gregorian_seconds_to_datetime(TotalSec),
    io_lib:format("~B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B.~2.10.0B",
      [Year, Month, Day, Hour, Minute, Second]).
