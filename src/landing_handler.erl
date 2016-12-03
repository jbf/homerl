-module(landing_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {}).

init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
	{ok, Req2} = cowboy_req:reply(200,
                                      local_http_utils:headers(),
                                      body(),
                                      Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.

body() ->
    [
        local_http_utils:pre_body()
      , sensors()
      , devices()
      , local_http_utils:post_body()
    ].

devices() ->
    [
        "<div id=devices><h3>Devices</h3><table>"
      , inner_device_list()
      , "</table></div>"
    ].

inner_device_list() ->
    Devices = device_directory:list_devices(),
    ["<tr><td>" ++ integer_to_list(Id) ++ "</td>" ++
         "<td>" ++ Name ++ "</td>" ++
         "<td><a href=/device?id=" ++
                 integer_to_list(Id) ++
                 "&action=start>Start</a></td>" ++
          "<td><a href=/device?id=" ++
                 integer_to_list(Id) ++
                 "&action=stop>Stop</a></td></tr>\n"
     || {_, Id, Name, _Type} <- Devices].

sensors() ->
    [
        "<div id=sensors><h3>Sensors</h3><table>"
      , inner_sensor_list()
      , "</table></div>"
    ].

inner_sensor_list() ->
    Sensors = sensor_directory:get_sensors(),
    ["<tr><td><a href=/sensor?id=" ++
                 integer_to_list(Id) ++
                 ">" ++
                 integer_to_list(Id) ++
                 ", " ++
                 atom_to_list(Type) ++
                 "</a></td></tr>\n" || {_, Id, Type} <- Sensors].
