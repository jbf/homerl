-module(device_control).

-export([execute_command/2]).

-include("device.hrl").

execute_command(Device = #device{cbmod = CBMod}, Command) ->
    CBMod:do_cmd(Device, Command);
execute_command(I, Command) when is_integer(I) ->
    Device = lists:keyfind(I, 2, device_directory:list_devices()),
    execute_command(Device, Command).
