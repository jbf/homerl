-module(data_store).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/0, start/1,
         start/0, stop/0, add_data/1
        ]).

%% gen_server Callback
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {tabs}).

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start(Args) ->
    gen_server:start({local, ?SERVER}, ?MODULE, Args, []).

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, stop).

add_data(DataPoint) ->
    gen_server:cast(?SERVER, {add_data, DataPoint}).

init([]) ->
    %Tab = ets:new(sensor_dir_table, [{keypos,2}]),
    {ok, #state{tabs=orddict:new()}}.

handle_cast({add_data, {Sensor, Type, _Value, _Time}}, State) ->
    Dict = State#state.tabs,
    {_Bucket, NewState} = case orddict:find({Sensor, Type}, Dict) of
                 {ok, Bkt} -> {Bkt, State};
                 error -> NewBucket = [],
                          NewDict = orddict:store({Sensor, Type}, NewBucket, Dict),
                          {NewBucket, State#state{tabs=NewDict}}
                         end,
    {noreply, NewState};
handle_cast(stop, State) ->
    {stop, normal, State}.

handle_call(_Unknown, _From, State) ->
    {reply, ok, State}.

handle_info(_Unknown, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

