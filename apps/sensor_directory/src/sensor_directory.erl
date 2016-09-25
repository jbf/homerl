-module(sensor_directory).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/0, start/1,
         start/0, stop/0, get_sensor/1,
         blacklisted_sensor/1, get_sensors/0
        ]).

%% gen_server Callback
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {tab, blacklist}).

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

get_sensors() ->
    gen_server:call(?SERVER, {get_sensors}).

get_sensor(SensorId) ->
    gen_server:call(?SERVER, {get_sensor, SensorId}).

blacklisted_sensor(SensorId) ->
    lists:member(SensorId, gen_server:call(?SERVER, {get_blacklist})).

init([]) ->
    Tab = ets:new(sensor_dir_table, [{keypos,2}]),
    Blacklist = application:get_env(?MODULE, blacklisted_sensors, []),
    {ok, #state{tab=Tab, blacklist=Blacklist}}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_call({get_sensor, SensorId}, _From, State) ->
    Tab = State#state.tab,
    Res = ets:lookup(Tab, SensorId),
    case Res of
        [] -> update_directory(Tab),
            lookup_no_update(Tab, SensorId, State);
        [Sensor] -> {reply, {ok, Sensor}, State}
    end;

handle_call({get_blacklist}, _From, State=#state{tab=_, blacklist=Blacklist}) ->
    {reply, Blacklist, State};

handle_call({get_sensors}, _From, State=#state{tab=Tab, blacklist=Blacklist}) ->
    All = ets:select(Tab, [{{sensor, '$0', '$1'},[], ['$_']}]),
    NonBlacklist = [X || X = {_Sensor, Id, _Capability} <- All,
                         false =:= lists:member(Id, Blacklist) ],
    {reply, NonBlacklist, State}.

lookup_no_update(Tab, SensorId, State) ->
    Res = ets:lookup(Tab, SensorId),
    case Res of
        [] -> {reply, {unknown}, State};
        [Sensor] -> {reply, {ok, Sensor}, State}
    end.

update_directory(Tab) ->
    RawInput = os:cmd("tdtool --list-sensors | sed -e s/\\=/\\ /g | awk '{print \"{\" $2 \",\" $8 \",\" $6 \"}\" }'"),
    Delimited = re:replace(RawInput, "\n", ",", [{return, iodata}, global]),
    Delimited2 = re:replace(Delimited, ",$", "", [{return, list}]),
    Undecoded = "[" ++ Delimited2 ++ "].",
    Input = decode(Undecoded),
    ets:insert(Tab, Input).

decode(Data) ->
    {ok, Tokens, _} = erl_scan:string(Data),
    {ok, Ret} = erl_parse:parse_term(Tokens),
    Ret.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
