-module(device_directory).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([parse_devices/0]).

-record(state, {devices}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

%% gen_server.

init([]) ->
  {ok, #state{}}.

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%% Internals
%%% TODO: Fix this
decode(Data) ->
    {ok, Tokens, _} = erl_scan:string(Data),
    {ok, Ret} = erl_parse:parse_term(Tokens),
    Ret.

parse_devices() -> decode(get_devices()).

get_devices() ->
  CMD = "tdtool --list-devices | awk 'BEGIN {FS=\" |=|\\t\"} NR == 1 {printf(\"[ \")} NR > 1 {printf(\"\\n ,\")}{printf(\" {device, %s, \\\"%s\\\"}\", $4,$6)} END {printf(\" ].\\n\")}'",
  %io:format("~s~n", [CMD]).
  os:cmd(CMD).

%% vim: tabstop=2 shiftwidth=2 expandtab
