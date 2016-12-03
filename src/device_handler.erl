-module(device_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
	{ok, Req2} = cowboy_req:reply(200,
            local_http_utils:headers([{<<"Cache-Control">>, <<"no-store, must-revalidate">>},
                     {<<"Expires">>, <<"0">>}]),
            body(Req),
            Req),
	{ok, Req2, State}.

body(Req) ->
  {BId, Req2} = cowboy_req:qs_val(<<"id">>, Req),
  Id = binary_to_integer(BId),
  {BAction, _Req3} = cowboy_req:qs_val(<<"action">>, Req2),
  Action = filter(BAction),
  device_control:execute_command(Id, Action),
  "<html><head><meta http-equiv=\"refresh\" content=\"2;url=/\" /></head><body>Ok!</body></html>".

filter(<<"start">>) -> start;
filter(<<"stop">>) -> stop;
filter(A) -> A.

terminate(_Reason, _Req, _State) ->
	ok.
