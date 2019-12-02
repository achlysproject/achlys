%%%-------------------------------------------------------------------
%%% @author Igor Kopestenski <igor.kopestenski@uclouvain.be>
%%%     [https://github.com/achlysproject/achlys]
%%% 2018, Universite Catholique de Louvain
%%% @doc
%%%
%%% @end
%%% Created : 08. Nov 2018 20:03
%%%-------------------------------------------------------------------
-module(achlys_sensor_commander).
-author("Igor Kopestenski <igor.kopestenski@uclouvain.be>").

-behaviour(gen_server).

-include ("achlys.hrl").

%% API
-export([start_link/0]).
-export([run_nav/0]).

%% gen_server callbacks
-export([init/1 ,
         handle_call/3 ,
         handle_cast/2 ,
         handle_info/2 ,
         terminate/2 ,
         code_change/3]).

-define(SERVER , ?MODULE).

%%====================================================================
%% Records
%%====================================================================

-record(state , {
    streamers :: map()
}).

-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok , Pid :: pid()} | ignore | {error , Reason :: term()}).
start_link() ->
    gen_server:start_link({local , ?SERVER} , ?MODULE , [] , []).

run_nav() ->
    Sup = whereis(achlys_pmod_worker_sup),
    supervisor:start_child(Sup, ?NAV_WORKER).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init([]) -> {ok , state()}).
init([]) ->
    ok = achlys_config:set(number, achlys_util:get_inet_least_significant()),
    Streamers = achlys_config:get(streamers),
    self() ! {setup_stream_workers},
    {ok , #state{streamers = Streamers}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term() , From :: {pid() , Tag :: term()} ,
                  State :: #state{}) ->
                     {reply , ignore , NewState :: #state{}}).
handle_call(_Request , _From , State) ->
    {reply , ignore , State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term() , State :: #state{}) -> {noreply , NewState :: #state{}}).
handle_cast(_Request , State) ->
    {noreply , State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term() , State :: #state{}) ->
    {noreply , NewState :: #state{}}).
handle_info({setup_stream_workers} , State) when is_map(State#state.streamers) ->
    logger:log(notice, "Initializing data stream workers ~n "),
    _ = [ self() ! {run, X} || X <- maps:keys(State#state.streamers)],

    {_Code, Val} = case check_streamers(State#state.streamers) of
        {ok, [Ks]} ->
            _ = maybe_run_workers([Ks]),
            {ok, [Ks]};
        {ok, [H|T]} ->
            _ = maybe_run_workers([H|T]),
            {ok, [H|T]};
        {error, Reason} ->
            {error, Reason}
    end,

    logger:log(notice, "Workers = ~p ~n", [Val]),

    {noreply , State};

handle_info({run, pmod_nav} , State) ->
    _ = run_nav(),
    {noreply , State};

handle_info(_Info , State) ->
    logger:log(notice, "Unhandled Info ~n "),
    {noreply , State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown , term()} | term()) ,
                State :: #state{}) -> term()).
terminate(_Reason , _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down , term()} , State :: #state{} ,
                  Extra :: term()) ->
                     {ok , NewState :: #state{}}).
code_change(_OldVsn , State , _Extra) ->
    {ok , State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
check_streamers(Streamers) ->
    try maps:keys(Streamers) of
        [Ks] ->
            {ok, [Ks]};
        [H|T] ->
            {ok, [H|T]}
    catch
        _:_ ->
            {error, no_streamers}
    end.

%% @private
% maybe_run_workers([Ks]) ->
maybe_run_workers(Ks) when is_list(Ks) ->
    RunningDevices = [ X || 
        {device, _Slot, X, _Pid, _Ref} <- grisp_devices:list()
        , lists:member(X, Ks) ],
    run_workers(RunningDevices, whereis(achlys_pmod_worker_sup)).

%% @private
run_workers([pmod_nav|T], Sup) ->
    _ = supervisor:start_child(Sup, ?NAV_WORKER),
    run_workers(T, Sup);
run_workers([pmod_als|T], Sup) ->
    _ = supervisor:start_child(Sup, ?ALS_WORKER),
    run_workers(T, Sup);
run_workers([H|T], Sup) ->
    logger:log(notice, "Worker not yet implemented ~p ~n ", [H]),
    run_workers(T, Sup);
run_workers([], _Sup) ->
    [].
