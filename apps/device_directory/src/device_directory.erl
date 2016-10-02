-module(device_directory).
-behaviour(gen_server).

-compile({parse_transform, lager_transform}).

%% API.
-export([start_link/0]).
-export([list_devices/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {devices}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

-spec list_devices(pid()) -> [_].
list_devices(Pid) ->
  gen_server:call(Pid, list_devices).

%% gen_server.

init([]) ->
  {ok, #state{}, 0}.

handle_call(list_devices, _From, S=#state{devices=Devs}) ->
  {reply, Devs, S};
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(timeout, #state{devices=undefined}) ->
  NewState = #state{devices=parse_devices()},
  {noreply, NewState};
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
  lager:info("devices found: " ++ CMD),
  os:cmd(CMD).

%% vim: tabstop=2 shiftwidth=2 expandtab
