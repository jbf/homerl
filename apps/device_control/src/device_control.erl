-module(device_control).

-export([execute_command/2]).

-include("device.hrl").

execute_command(Device = #device{cbmod = CBMod}, Command) ->
    CBMod:do_cmd(Device, Command).
