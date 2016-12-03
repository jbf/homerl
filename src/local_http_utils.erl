-module(local_http_utils).

-export([ headers/0, headers/1, pre_body/0, post_body/0 ]).

headers() ->
    [
        {<<"content-type">>, <<"text/html; charset=utf-8">>}
      , {<<"server">>, <<"yaws">>}
    ].

headers(HDR = [_|_]) ->
    HDR ++
    [
        {<<"content-type">>, <<"text/html; charset=utf-8">>}
      , {<<"server">>, <<"yaws">>}
    ].

pre_body() ->
    "<html><head></head><body>".

post_body() ->
    "</body></html>".
