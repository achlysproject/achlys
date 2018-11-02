%%%-------------------------------------------------------------------
%% @author Igor Kopestenski <igor.kopestenski@uclouvain.be>
%%   [https://github.com/Laymer/achlys/]
%% @doc The Pmod_NAV worker server.
%% The general purpose of this worker is to gather
%% and process sensor data from one of the 3 components
%% available on the Pmod_NAV :
%%
%%   - Accelerometer => acc
%%   - Gyroscope => alt
%%   - Magnetometer => mag
%%
%%   Data can be retrieved as follows :
%%
%%   [Temperature] = pmod_nav:read(acc, [out_temp]).
%%
%%   Where <em>acc</em> is the component providing the
%%   data and <em>[out_temp]</em> is the list of registers
%%   that is read.
%%   @see pmod_nav. <b>Pmod_NAV</b>
%%
%% @end
%%%-------------------------------------------------------------------

-module(achlys_pmod_nav_worker).
-author("Igor Kopestenski <igor.kopestenski@uclouvain.be>").

-behaviour(gen_server).

-include("achlys.hrl").

%%====================================================================
%% API
%%====================================================================

-export([start_link/1]).
-export([run/0]).
-export([get_crdt/0]).
-export([get_table/0]).

%%====================================================================
%% Gen Server Callbacks
%%====================================================================

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         handle_continue/2,
         terminate/2,
         code_change/3]).

%%====================================================================
%% Records
%%====================================================================

-record(state, {
  crdt                    :: any(),
  gc_interval             :: pos_integer(),
  poll_interval           :: pos_integer(),
  mean_interval           :: pos_integer(),
  table                   :: any()
}).

-type state() :: #state{}.

%% Configuration parameters
-type nav_config()        :: #{table := atom()
                            , gc_interval := pos_integer()
                            , poll_interval := pos_integer()
                            , aggregation_trigger := pos_integer() }.

%%====================================================================
%% API
%%====================================================================

%% @doc starts the pmod_nav process using the configuration
%% given in the sys.config file.
-spec start_link(nav_config()) ->
  {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link(NavConfig) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, NavConfig, []).

%% @doc declares a Lasp variable for temperature aggregates
%% and sets triggers for handlers after intervals have expired.
-spec run() -> ok.
run() ->
    gen_server:cast(self(), run).

%% @doc Returns the current view of the contents in the temperatures
%% Lasp variable.
-spec get_crdt() -> ok.
get_crdt() ->
    gen_server:cast(self(), get_crdt).

%% @doc Returns the current view of the contents in the temperatures
%% Lasp variable.
-spec get_table() -> ok.
get_table() ->
    gen_server:cast(self(), get_table).

%%====================================================================
%% Gen Server Callbacks
%%====================================================================

%% @private
-spec init(nav_config()) -> {ok, state()}.
init(NavConfig) ->
    #{table := T
    , gc_interval := GC
    , poll_interval := P
    , aggregation_trigger := Agg} = NavConfig,

    T = ets:new(T, [ordered_set
    , public
    , named_table
    , {heir, whereis(achlys_sup), []}
    ]),
    M = (P * Agg),

    erlang:send_after(GC,self(),gc),

    {ok, #state{gc_interval = GC
        , poll_interval = P
        , mean_interval = M
        , table = T}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.


%%--------------------------------------------------------------------

handle_cast(run, State) ->
    _Remotes = achlys_util:clusterize(),
    Id = achlys_util:declare_crdt(temp, state_awset),
    logger:log(notice, "Declared CRDT ~p for temperature aggregates ~n", [Id]),

    erlang:send_after(State#state.poll_interval,self(),poll),
    erlang:send_after(State#state.mean_interval,self(),mean),

    {noreply, State#state{crdt = Id}};

%%--------------------------------------------------------------------

handle_cast(get_crdt, State) ->
    {ok, S} = lasp:query(State#state.crdt),
    logger:log(notice, "CRDT ~p temperature aggregates ~n", [sets:to_list(S)]),
    {noreply, State};

%%--------------------------------------------------------------------

handle_cast(get_table, State) ->
    logger:log(notice, "Table info : ~p ~n", [ets:info(State#state.table)]),
    {noreply, State};

%%--------------------------------------------------------------------

handle_cast(mean, State) ->
    Mean = get_temp_mean(State#state.table),
    _ = lasp:update(State#state.crdt, {add, Mean}, self()),
    {noreply, State};

%%--------------------------------------------------------------------

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------

%% @doc fetches the temperature value from the {@link pmod_nav} sensor
%% and stores it in the corresponding ETS table. It is paired with
%% the {@link erlang:monotonic_time/0} to guarantee unique keys.
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
handle_info(poll, State) ->
  Temp = pmod_nav:read(acc, [out_temp]),
  true = ets:insert_new(State#state.table, {erlang:monotonic_time(), Temp}),
  erlang:send_after(State#state.poll_interval,self(),poll),
  {noreply, State};

%%--------------------------------------------------------------------

handle_info(gc, State) ->
    ok = achlys_util:do_gc(),
    erlang:send_after(State#state.gc_interval,self(),gc),
    {noreply, State};

%%--------------------------------------------------------------------

handle_info(mean, State) ->
    Mean = get_temp_mean(State#state.table),
    _ = lasp:update(State#state.crdt, {add, Mean}, self()),
    erlang:send_after(State#state.mean_interval,self(),mean),
    {noreply, State};

%%--------------------------------------------------------------------

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------

%% This function is called by a gen_server process
%% whenever a previous callback returns {continue, Continue}.
%% handle_continue/2 is invoked immediately after the previous callback,
%% which makes it useful for performing work after initialization
%% or for splitting the work in a callback in multiple steps,
%% updating the process state along the way.

%%--------------------------------------------------------------------

handle_continue(_Continue, State) ->
  % {noreply,NewState} | {noreply,NewState,Timeout}
  % | {noreply,NewState,hibernate}
  % | {noreply,NewState,{continue,Continue}}
  % | {stop,Reason,NewState}
    {noreply, State}.

%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

-spec get_temp_mean(atom() | ets:tid()) -> {number(),float()}.
%% @doc Returns the temperature average
%% based on entries in the
get_temp_mean(Tab) ->
    Sum = ets:foldl(fun
      (Elem, AccIn) ->
        {_, [Temp]} = Elem,
        Temp + AccIn
    end, 0, Tab),
    Len = ets:info(Tab, size),
    {Len, (Sum / Len)}.
