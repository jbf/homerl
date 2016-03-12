-module(data_store).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/0, start/1,
         start/0, stop/0, add_data/1, get_data/1
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

get_data(Key) ->
    gen_server:call(?SERVER, {get_data, Key}).

init([]) ->
    %Tab = ets:new(sensor_dir_table, [{keypos,2}]),
    {ok, #state{tabs=orddict:new()}}.

handle_cast({add_data, Data = {Sensor, Type, _Value, _Time}}, State) ->
    Buckets = State#state.tabs,
    OldBucket = get_or_create_bucket({Sensor, Type}, Buckets),
    NewBucket = bucket_add_data(OldBucket, Data),
    NewBuckets = store_bucket(Buckets, {Sensor, Type}, NewBucket),
    NewState = State#state{tabs=NewBuckets},
    {noreply, NewState};
handle_cast(stop, State) ->
    {stop, normal, State}.

get_or_create_bucket(Key, Buckets) ->
    Bucket = orddict:find(Key, Buckets),
    case Bucket of
        error -> [];
        {ok, B} -> B
    end.

bucket_add_data(OldBucket, Data) ->
    [Data | OldBucket].

store_bucket(Buckets, Key, Bucket) ->
    orddict:store(Key, Bucket, Buckets).

handle_call({get_data, Key}, _From, State = #state{tabs = T}) ->
    Bucket = orddict:find(Key, T),
    case Bucket of
        {ok, B} -> {reply, get_data2(B), State};
        error -> {reply, [], State}
    end;

handle_call(_Unknown, _From, State) ->
    {reply, ok, State}.

get_data2(Bucket) -> Bucket.

handle_info(_Unknown, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

