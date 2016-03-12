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
-record(bucket, {timestamp,
                 tab}).

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
    {ok, #state{tabs=make_buckets()}}.

make_buckets() -> orddict:new().

handle_cast({add_data, Data = {Sensor, Type, _Value, _Time}}, State) ->
    Buckets = State#state.tabs,
    Key = {Sensor, Type},
    OldBucket = get_bucket(Key, Buckets),
    NewBucket = bucket_add_data(OldBucket, Data),
    NewBuckets = store_bucket(Buckets, Key, NewBucket),
    NewState = State#state{tabs=NewBuckets},
    {noreply, NewState};
handle_cast(stop, State) ->
    {stop, normal, State}.

get_bucket(Key, Buckets) ->
    Bucket = orddict:find(Key, Buckets),
    case Bucket of
        error -> empty;
        {ok, B} -> B
    end.

bucket_add_data(OldBucket, Data) ->
    case OldBucket of
        #bucket{tab = T} -> add_maybe_gc(T, Data),
                            OldBucket;
        empty ->
            new_bucket_with_data(Data)
    end.

add_maybe_gc(T, Data) ->
    _Size = ets:info(T, size),
    %TODO: add gc here
    ets:insert(T, Data).

new_bucket_with_data(Data = {Id, T, _V, TimeStamp}) ->
    TabStr = "bucket_tab_" ++ integer_to_list(Id) ++ "_" ++ atom_to_list(T),
    Tab = ets:new(list_to_atom(TabStr), [ordered_set, {keypos, 4}]),
    ets:insert(Tab, Data),
    #bucket{timestamp = TimeStamp, tab = Tab}.

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

get_data2(#bucket{tab = T}) ->
    ets:tab2list(T).

handle_info(_Unknown, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

