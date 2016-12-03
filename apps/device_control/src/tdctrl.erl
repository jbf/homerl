-module(tdctrl).

-export([do_cmd/2]).

-compile({parse_transform, lager_transform}).

-include("device.hrl").

%-behaviour(device_controller).

do_cmd(Device = #device{cbmod=tdctrl}, start) ->
    internal_do(Device, "--on");
do_cmd(Device = #device{cbmod=tdctrl}, stop) ->
    internal_do(Device, "--off").

internal_do(_Device = #device{id = Id, cbmod=tdctrl}, RawCmd) ->
    Cmd = io_lib:format("tdtool ~s ~w", [RawCmd, Id]),
    lager:info("Sending: ~s", [Cmd]),
    Resp = os:cmd(Cmd),
    lager:info("Response: ~s", [Resp]),
    ok.
