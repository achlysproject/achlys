%%%-------------------------------------------------------------------
%%% @author Igor Kopestenski <igor.kopestenski@uclouvain.be>
%%%   [https://github.com/achlysproject/achlys/]
%%% @doc The Pmod_ALS worker server.
%%% The general purpose of this worker is to gather
%%% and process sensor data the pmod_als module :
%%%
%%%   - Ambient Light (raw)
%%%   - Ambient Light percentage
%%%
%%%   Data can be retrieved as follows :
%%%
%%%   Raw = pmod_als:read().
%%%   Percentage = pmod_als:percentage().
%%%
%%%   @see pmod_als. <b>Pmod_ALS</b>
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(achlys_pmod_als_worker).
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
-define(PMOD_ALS_SLOT , spi2).

%%====================================================================
%% Records
%%====================================================================

-record(state , {
    measures :: map(),
    number :: binary()
}).

-type state() :: #state{}.

-type crdt() :: {bitstring(), atom()}.

%% Configuration parameters
% -type nav_config() :: #{table := atom()
%     , poll_interval := pos_integer()
%     , aggregation_trigger := pos_integer()
% }.
%
-type pmod_als_status() :: {ok , pmod_als}
    | {error , no_device | no_pmod_als | unknown}.

%%====================================================================
%% API
%%====================================================================

% @doc starts the pmod_als process using the configuration
% given in the sys.config file.
-spec start_link() ->
  {ok , pid()} | ignore | {error , {already_started , pid()} | term()}.
start_link() ->
    gen_server:start_link({local , ?MODULE} , ?MODULE , [] , []).

%% @doc declares a Lasp variable for aggregates
%% and sets triggers for handlers after intervals have expired.
-spec run() -> ok.
run() ->
    gen_server:cast(?SERVER , run).

%% @doc Returns the current view of the contents in Lasp variable.
% -spec get_crdt() -> crdt().
-spec get_crdt(atom()) -> list().
get_crdt(Name) ->
    gen_server:call(?SERVER , {get_crdt, Name} , ?TEN).

%% @doc Returns the current view of the contents in Lasp variable.
-spec get_table(atom()) -> ok.
get_table(Name) ->
    gen_server:call(?SERVER , {get_table, Name}).

%%====================================================================
%% Gen Server Callbacks
%%====================================================================

% @private
-spec init([]) -> {ok , state()}.
init([]) ->
    Streamers = achlys_config:get(streamers, #{}),
    Num = achlys_config:get(number, 0),
    MeasuresMap = mapz:deep_get([?SERVER], Streamers),
    erlang:send_after(?ONE , ?SERVER , {measure, MeasuresMap}),
    {ok, #state{measures = MeasuresMap, number = Num}}.

%%--------------------------------------------------------------------

% @private
handle_call({get_crdt, Name} , _From , State) ->
    Id = mapz:deep_get([Name, crdt], State#state.measures),
    % {ok , S} = lasp:query(State#state.crdt) ,
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
        Id = achlys_util:declare_crdt(K , state_gset),
        % Id = achlys_util:declare_crdt(K , state_awset),
        % Id = maybe_declare_crdt(K , state_awset_ps),

        V2 = mapz:deep_put([crdt], Id , V1),
        T = achlys_util:create_table(K),

        V3 = mapz:deep_put([table], T , V2),
        #{poll_interval := P
        , aggregation_trigger := A} = V3,
        erlang:send_after(((P * A) + ?THREE), ?SERVER, {mean, K}),
        V3
    end, I),
    erlang:send_after(?ONE , ?SERVER , poll) ,
    {noreply , State#state{measures = NewState}};

%%--------------------------------------------------------------------

% @private
handle_cast(_Msg , State) ->
    {noreply , State}.

%%--------------------------------------------------------------------

%% @doc fetches the values from the {@link pmod_als} sensor
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

handle_info(light , State) ->
    Res = maybe_get_light() ,
    case Res of
        {ok , Light} when is_number(Light) ->
            % true = ets:insert_new(State#state.measures , {?TIME , [Temp]});
            true = ets:insert_new(light , {erlang:monotonic_time() , Light});
        _ ->
            logger:log(notice , "Could not fetch light : ~p ~n" , [Res])
    end ,

    erlang:send_after(mapz:deep_get([light, poll_interval], State#state.measures)
    , ?SERVER
    , light) ,

    {noreply , State};
%
% handle_info(pressure , State) ->
%     Res = maybe_get_press() ,
%     case Res of
%         {ok , [Press]} when is_number(Press) ->
%             % true = ets:insert_new(State#state.measures , {?TIME , [Press]});
%             true = ets:insert_new(pressure , {?TIME , [Press]});
%         _ ->
%             logger:log(notice , "Could not fetch pressure : ~p ~n" , [Res])
%     end ,
%     erlang:send_after(mapz:deep_get([pressure, poll_interval],State#state.measures)
%     , ?SERVER
%     , pressure) ,
%     {noreply , State};

%%--------------------------------------------------------------------

handle_info({mean, Val} , State) ->
    #{table := T
    , crdt := C
    , poll_interval := P
    , aggregation_trigger := A} = mapz:deep_get([Val],State#state.measures),
    Len = ets:info(Val, size),
    _ = case Len >= A of
            true ->
                % Mean = get_mean(Val) ,
                % {ok, {C2, _, _, _}} = lasp:update(C , {add , {State#state.number , T, Mean}} , self());
                {Sample, Mean} = get_mean(Val) ,
                {ok, {C2, _, _, _}} = lasp:update(C , {add , {State#state.number , Sample, erlang:round(Mean)}} , self());
            _ ->
                logger:log(notice , "Could not compute mean with ~p values ~n" , [Len])
        end ,
    erlang:send_after((P * A) , ?SERVER , {mean, Val}) ,
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
    ok = ets:tab2file(light, "light"),
    ok.

%%--------------------------------------------------------------------

code_change(_OldVsn , State , _Extra) ->
    {ok , State}.


%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Returns the ambient light average
%% based on entries in the ETS table
-spec get_mean(atom() | ets:tid()) -> {number() , float()}.
get_mean(Tab) ->
    Sum = ets:foldl(fun
                        (Elem , AccIn) ->
                            {_ , Light} = Elem ,
                            Light + AccIn
                    end , 0 , Tab) ,
    Len = ets:info(Tab , size) ,
    {Len , (Sum / Len)}.

%% @doc Returns the current temperature
%% if a pmod_als module is active on slot SPI1
-spec maybe_get_light() -> {ok , 0..255} | pmod_als_status().
maybe_get_light() ->
    {Code , Val} = is_pmod_als_alive() ,
    case {Code , Val} of
        {ok , pmod_als} ->
            {ok , pmod_als:read()};
        _ ->
            {Code , Val}
    end.

%% @doc Checks the SPI2 slot of the GRiSP board
%% for presence of a Pmod_ALS module.
-spec is_pmod_als_alive() -> pmod_als_status().
is_pmod_als_alive() ->
    try grisp_devices:slot(?PMOD_ALS_SLOT) of
        {device , ?PMOD_ALS_SLOT , Device , _Pid , _Ref} when Device =:= pmod_als ->
            {ok , pmod_als};
        {device , ?PMOD_ALS_SLOT , Device , _Pid , _Ref} when Device =/= pmod_als ->
            {error , no_pmod_als}
    catch
        error:{no_device_connected , ?PMOD_ALS_SLOT} ->
            {error , no_device};
        _:_ ->
            {error , unknown}
    end.
%   error:{badmatch, {device, ?PMOD_ALS_SLOT, pmod_als, _Pid, _Ref}} ->
%     {error, no_pmod_als};
