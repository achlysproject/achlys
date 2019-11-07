%%%-------------------------------------------------------------------
%%% @author Igor Kopestenski <igor.kopestenski@uclouvain.be>
%%%   [https://github.com/Laymer/achlys/]
%%% @doc The Pmod_NAV worker server.
%%% The general purpose of this worker is to gather
%%% and process sensor data from one of the 3 components
%%% available on the Pmod_NAV :
%%%
%%%   - Accelerometer => acc
%%%   - Gyroscope => alt
%%%   - Magnetometer => mag
%%%
%%%   Data can be retrieved as follows :
%%%
%%%   [Temperature] = pmod_nav:read(acc, [out_temp]).
%%%
%%%   Where <em>acc</em> is the component providing the
%%%   data and <em>[out_temp]</em> is the list of registers
%%%   that is read.
%%%   @see pmod_nav. <b>Pmod_NAV</b>
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(achlys_pmod_nav_worker).
-author("Igor Kopestenski <igor.kopestenski@uclouvain.be>").

-behaviour(gen_server).

-include("achlys.hrl").

%%====================================================================
%% API
%%====================================================================

-export([start_link/0]).
% -export([start_link/1]).
-export([run/0]).
-export([get_crdt/1]).
-export([get_table/1]).

%%====================================================================
%% Gen Server Callbacks
%%====================================================================

-export([init/1 ,
         handle_call/3 ,
         handle_cast/2 ,
         handle_info/2 ,
         handle_continue/2 ,
         terminate/2 ,
         code_change/3]).

%%====================================================================
%% Macros
%%====================================================================

-define(SERVER , ?MODULE).
-define(PMOD_NAV_SLOT , spi1).
-define(TIME , erlang:monotonic_time()).

%%====================================================================
%% Records
%%====================================================================

-record(state , {
    measures :: map(),
    aggregations :: map(),
    number :: binary(),
    additions :: pos_integer(),
    removals :: pos_integer()
}).

-type state() :: #state{}.

-type crdt() :: {bitstring(), atom()}.

%% Configuration parameters
-type nav_config() :: #{table := atom()
    , poll_interval := pos_integer()
    , aggregation_trigger := pos_integer()
}.

-type pmod_nav_status() :: {ok , pmod_nav}
    | {error , no_device | no_pmod_nav | unknown}.

%%====================================================================
%% API
%%====================================================================

% @doc starts the pmod_nav process using the configuration
% given in the sys.config file.
-spec start_link() ->
  {ok , pid()} | ignore | {error , {already_started , pid()} | term()}.
start_link() ->
    gen_server:start_link({local , ?MODULE} , ?MODULE , [] , []).

%% @doc declares a Lasp variable for temperature aggregates
%% and sets triggers for handlers after intervals have expired.
-spec run() -> ok.
run() ->
    gen_server:cast(?SERVER , run).

%% @doc Returns the current view of the contents in the temperatures
%% Lasp variable.
% -spec get_crdt() -> crdt().
-spec get_crdt(atom()) -> list().
get_crdt(Name) ->
    gen_server:call(?SERVER , {get_crdt, Name} , ?TEN).

%% @doc Returns the current view of the contents in the temperatures
%% Lasp variable.
-spec get_table(atom()) -> ok.
get_table(Name) ->
    gen_server:call(?SERVER , {get_table, Name}).

%%====================================================================
%% Gen Server Callbacks
%%====================================================================

% @private
-spec init([]) -> {ok , state()}.
init([]) ->
    _ = rand:seed(exrop),
    Streamers = achlys_config:get(streamers, #{}),
    MeasuresMap = mapz:deep_get([pmod_nav], Streamers),
    erlang:send_after(?ONE , ?SERVER , {measure, MeasuresMap}),
    L = [ {X, 1} || X <- maps:keys(MeasuresMap) ],
    {ok, #state{
        measures = MeasuresMap,
        additions = 0,
        removals = 0,
        aggregations = maps:from_list(L)}}.

%%--------------------------------------------------------------------

% @private
handle_call({get_crdt, Name} , _From , State) ->
    Id = mapz:deep_get([Name, crdt], State#state.measures),
    {ok , S} = lasp:query(Id) ,
    L = sets:to_list(S) ,
    {reply , L , State};

%%--------------------------------------------------------------------

% @private
handle_call({get_table, Name} , _From , State) ->
    Tab = mapz:deep_get([Name, table], State#state.measures),
    % Match = ets:match(State#state.table , '$1') ,
    Match = ets:match(Tab , '$1') ,
    logger:log(notice , "Table info : ~p ~n" , [ets:info(Tab)]) ,
    {reply , Match , State};

%%--------------------------------------------------------------------

% @private
handle_call(_Request , _From , State) ->
    {reply , ignored , State}.

%%--------------------------------------------------------------------

% @private
handle_cast(run , State) ->
    logger:log(notice , "Declared CRDTs for aggregates ~n"),
    I = maps:iterator(State#state.measures),

    NewState = maps:map(fun
      (K, V1) when is_map(V1) ->
        {SetId, _CardinalityId} = achlys_util:get_variable_identifier(K),
        {ok, {Id,_,_,_}} = lasp:declare({SetId , state_awset}, state_awset),

        V2 = mapz:deep_put([crdt], Id , V1),
        T = achlys_util:create_table(K),

        V3 = mapz:deep_put([table], T , V2),
        #{poll_interval := P
        , aggregation_trigger := A} = V3,
        erlang:send_after(((P * A) + ?THREE), ?SERVER, {mean, K}),
        % erlang:send_after(((P * A * A) + ?FIVE), ?SERVER, {flush, K}),
        V3
    end, I),
    erlang:send_after(?ONE , ?SERVER , poll) ,
    {noreply , State#state{measures = NewState}};

%%--------------------------------------------------------------------

% @private
handle_cast(_Msg , State) ->
    {noreply , State}.

%%--------------------------------------------------------------------

%% @doc fetches the values from the {@link pmod_nav} sensor
%% and stores them in the corresponding ETS table. It is paired with
%% the {@link erlang:monotonic_time/0} to guarantee unique keys.
%% The first call is used to redirect towards implemented handlers
%% e.g. temperature, pressure.
%% For large amounts of sensor data
%% e.g. accumulated for a long time and being larger than
%% the maximum available memory, an alternative would be to use the
%% {@link dets} storage module. They can also be combined as described
%% below.
%%
%% From OTP documentation :
%%
%% Dets tables provide efficient file-based Erlang term storage.
%% They are used together with ETS tables when fast access
%% needs to be complemented with persistency.
handle_info(poll , State) ->
    [ erlang:send_after(mapz:deep_get([K, poll_interval],
        State#state.measures),
        ?SERVER, K)
            || K <- maps:keys(State#state.measures)],
    {noreply , State};

handle_info(temperature , State) ->
    Res = maybe_get_temp() ,
    case Res of
        {ok , [Temp]} when is_number(Temp) ->
            true = ets:insert_new(temperature , {?TIME , erlang:round(Temp)});
        _ ->
            logger:log(notice , "Could not fetch temperature : ~p ~n" , [Res])
    end ,
    erlang:send_after(mapz:deep_get([temperature, poll_interval],State#state.measures)
    , ?SERVER
    , temperature) ,
    {noreply , State, hibernate};

handle_info(pressure , State) ->
    Res = maybe_get_press() ,
    case Res of
        {ok , [Press]} when is_number(Press) ->
            true = ets:insert_new(pressure , {?TIME , erlang:round(Press)});
        _ ->
            logger:log(notice , "Could not fetch pressure : ~p ~n" , [Res])
    end ,
    erlang:send_after(mapz:deep_get([pressure, poll_interval],State#state.measures)
    , ?SERVER
    , pressure) ,
    {noreply , State, hibernate};

%%--------------------------------------------------------------------

handle_info({mean, Val} , State) ->
    #{table := T
    , crdt := C
    , poll_interval := P
    , aggregation_trigger := A} = mapz:deep_get([Val],State#state.measures),
    Len = ets:info(Val, size),
    _ = case Len >= A of
            true ->
                {_Sample, Mean} = get_mean(Val) ,
                ComputedSample = (maps:get(Val, State#state.aggregations) * A) ,
                {ok, {C2, _, _, _}} = lasp:update(
                    C ,
                    {add , {ComputedSample , erlang:round(Mean)}} ,
                    self());
            _ ->
                logger:log(notice , "Could not compute mean with ~p values ~n" , [Len])
        end ,
    erlang:send_after((P * A) , ?SERVER , {mean, Val}) ,
    Increment = maps:update_with(Val,
        fun(V) -> V + 1 end,
        State#state.aggregations),
    ok = achlys_cleaner:flush_table(T) ,
    {noreply , State#state{aggregations = Increment}};

%%--------------------------------------------------------------------

handle_info({flush, Val} , State) ->
    #{crdt := C
    , poll_interval := P
    , aggregation_trigger := A} = mapz:deep_get([Val],State#state.measures),
    {ok, Set} = lasp:query(C),
    L = sets:to_list(Set),
    Last = lists:last(lists:usort(L)),
    ets:delete(node(), C),
    %% TODO : compute cumulative weighted average.
    {ok, {C2, _, _, _}} = lasp:update(
        C ,
        {rmv_all , L} ,
        self()),
    {ok, {C3, _, _, _}} = lasp:update(
        C2 ,
        {add , Last} ,
        self()),
    erlang:send_after((P * A * A) , ?SERVER , {flush, Val}) ,
    {noreply , State};

%%--------------------------------------------------------------------

handle_info({measure, Map} , State) ->
    logger:log(notice , "Pmod measuring ~p values ~n" , [Map]),
    {noreply , State};

%%--------------------------------------------------------------------

handle_info(Info , State) ->
    logger:log(notice , "Info ~p values ~n" , [Info]),
    {noreply , State}.

%%--------------------------------------------------------------------

%% This function is called by a gen_server process
%% whenever a previous callback returns {continue, Continue}.
%% handle_continue/2 is invoked immediately after the previous callback,
%% which makes it useful for performing work after initialization
%% or for splitting the work in a callback in multiple steps,
%% updating the process state along the way.

%%--------------------------------------------------------------------

handle_continue(_Continue , State) ->
    % {noreply,NewState} | {noreply,NewState,Timeout}
    % | {noreply,NewState,hibernate}
    % | {noreply,NewState,{continue,Continue}}
    % | {stop,Reason,NewState}
    {noreply , State}.

%%--------------------------------------------------------------------

terminate(_Reason , _State) ->
    % ok = ets:tab2file(temperature, "temperature"),
    % ok = ets:tab2file(pressure, "pressure"),
    dets:sync(node()),
    ok.

%%--------------------------------------------------------------------

code_change(_OldVsn , State , _Extra) ->
    {ok , State}.


%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Returns the temperature average
%% based on entries in the ETS table
-spec get_mean(atom() | ets:tid()) -> {number() , float()}.
get_mean(Tab) ->
    Sum = ets:foldl(fun
                        (Elem , AccIn) ->
                            {_ , Temp} = Elem ,
                            Temp + AccIn
                    end , 0 , Tab) ,
    Len = ets:info(Tab , size) ,
    {Len , (Sum / Len)}.

%% @doc Returns the current temperature
%% if a Pmod_NAV module is active on slot SPI1
-spec maybe_get_temp() -> {ok , [float()]} | pmod_nav_status().
maybe_get_temp() ->
    {Code , Val} = is_pmod_nav_alive() ,
    case {Code , Val} of
        {ok , pmod_nav} ->
            {ok , pmod_nav:read(acc , [out_temp])};
        _ ->
            {Code , Val}
    end.

%% @doc Returns the current pressure
%% if a Pmod_NAV module is active on slot SPI1
-spec maybe_get_press() -> {ok , [float()]} | pmod_nav_status().
maybe_get_press() ->
    {Code , Val} = is_pmod_nav_alive() ,
    case {Code , Val} of
        {ok , pmod_nav} ->
            {ok , pmod_nav:read(alt , [press_out])};
        _ ->
            {Code , Val}
    end.

%% @doc Checks the SPI1 slot of the GRiSP board
%% for presence of a Pmod_NAV module.
-spec is_pmod_nav_alive() -> pmod_nav_status().
is_pmod_nav_alive() ->
    try grisp_devices:slot(?PMOD_NAV_SLOT) of
        {device , ?PMOD_NAV_SLOT , Device , _Pid , _Ref} when Device =:= pmod_nav ->
            {ok , pmod_nav};
        {device , ?PMOD_NAV_SLOT , Device , _Pid , _Ref} when Device =/= pmod_nav ->
            {error , no_pmod_nav}
    catch
        error:{no_device_connected , ?PMOD_NAV_SLOT} ->
            {error , no_device};
        _:_ ->
            {error , unknown}
    end.
