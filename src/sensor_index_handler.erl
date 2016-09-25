-module(sensor_index_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {}).

init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
	{ok, Req2} = cowboy_req:reply(200, [
                {<<"content-type">>, <<"text/html; charset=utf-8">>},
                {<<"server">>, <<"yaws">>}
            ],
            html_sensor_list(),
            Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.

html_sensor_list() -> [
    "<html><head></head><body><table>",
    inner_sensor_list(),
    "</table></body></html>"].

inner_sensor_list() ->
    Sensors = sensor_directory:get_sensors(),
    ["<tr><td><a href=/sensor?id=" ++
                 integer_to_list(Id) ++
                 ">" ++
                 integer_to_list(Id) ++
                 ", " ++
                 atom_to_list(Type) ++
                 "</a></td></tr>\n" || {_, Id, Type} <- Sensors].
