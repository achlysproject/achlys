%%%-------------------------------------------------------------------
%%% @author Igor Kopestenski <igor.kopestenski@uclouvain.be>
%%%     [https://github.com/achlysproject/achlys]
%%%
%%% @doc The garbage collection worker.
%%% The general purpose of this worker is to perform
%%% explicit calls to the Erlang @link erlang:garbage_collect
%%% function. The GC procedure itself generates overhead if
%%% performed too often, but has shown improvements in stability
%%% on GRiSP boards when intervals around 30 to 60 seconds
%%%
%%% were used. The interval can be configured using the following
%%% parameter in a sys.config file :
%%%
%%% {achlys,
%%%     {gc_interval, 30000}
%%% }
%%%
%%% Where <em>gc_interval</em> is set to 30 seconds
%%% @end
%%% Created : 06. Nov 2018 21:47
%%%-------------------------------------------------------------------
-module(achlys_cleaner).
-author("Igor Kopestenski <igor.kopestenski@uclouvain.be>").

-behaviour(gen_server).

%% API
-export([start_link/0]).
% TODO : add API function to clean ETS table T upon request
-export([flush_table/1]).

%% gen_server callbacks
-export([init/1 ,
         handle_call/3 ,
         handle_cast/2 ,
         handle_info/2 ,
         handle_continue/2 ,
         terminate/2 ,
         code_change/3]).

-define(SERVER , ?MODULE).

-record(state , {
    gc_interval :: pos_integer()
}).

-type state() :: #state{}.

%% Configuration parameters
-type gc_config() :: pos_integer().

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Delete all objects in a given ETS table.
-spec flush_table(Table :: atom()) -> ok.
flush_table(Table) ->
    gen_server:cast(?SERVER , {flush_table, Table}).


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
-spec(init(Args :: term()) -> {ok , State :: state()}).
init(_Args) ->
    Interval = achlys_config:get(gc_interval, 45000),
    schedule_gc(Interval),
    _Res = schedule_dets_sync(achlys_config:get(sync_dets_interval, 3000)),
    {ok , #state{ gc_interval = Interval }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term() , From :: {pid() , Tag :: term()} ,
                  State :: #state{}) ->
                     {reply , ok , NewState :: #state{}}).
                     % {reply , Reply :: term() , NewState :: #state{}} |
                     % {reply , Reply :: term() , NewState :: #state{} , timeout() | hibernate} |
                     % {noreply , NewState :: #state{}} |
                     % {noreply , NewState :: #state{} , timeout() | hibernate} |
                     % {stop , Reason :: term() , Reply :: term() , NewState :: #state{}} |
                     % {stop , Reason :: term() , NewState :: #state{}}).
handle_call(_Request , _From , State) ->
    {reply , ok , State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term() , State :: #state{}) ->
    {noreply , NewState :: #state{}}).
    % {noreply , NewState :: #state{}} |
    % {noreply , NewState :: #state{} , timeout() | hibernate} |
    % {stop , Reason :: term() , NewState :: #state{}}).
handle_cast({flush_table, Table} , State) ->
    case application:get_env(lasp, storage_backend) of
        {ok, lasp_dets_storage_backend} ->
            _ = case dets:info(Table) of
                undefined ->
                    logger:log(info, "No DETS table ~p, flushing not necessary ~n", [Table]);
                _ ->
                    ok = dets:delete_all_objects(Table) ,
                    logger:log(notice , "Flushed DETS table ~p ~n" , [Table])
            end;
        {ok, lasp_ets_storage_backend} ->
            _ = case ets:whereis(Table) of
                undefined ->
                    logger:log(error , "Cannot flush undefined table ~p ~n" , [Table]);
                Ref ->
                    logger:log(notice , "Flushing ETS table ~p ~n" , [Table]) ,
                    true = ets:delete_all_objects(Ref)
            end;
        _ ->
            logger:log(error , "Cannot flush table  ~p ~n" , [Table])
    end ,
    {noreply , State};
handle_cast(_Request , State) ->
    {noreply , State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling the garbage collection periodical triggers.
%% @end
%%--------------------------------------------------------------------
handle_info(gc , State) ->
    ok = achlys_util:do_gc() ,
    erlang:send_after(State#state.gc_interval , ?SERVER , gc) ,
    {noreply , State};
%%--------------------------------------------------------------------
handle_info(dets_sync , State) ->
    N = node(),
    _ = case dets:info(N) of
        undefined ->
            logger:log(info, "No DETS table ~p, syncing not necessary ~n", [N]);
        _ ->
            ok = dets:sync(N) ,
            logger:log(info, "Synced DETS table ~p ~n", [N])
    end,
    erlang:send_after(3000 , ?SERVER , dets_sync) ,
    {noreply , State};

handle_info(_Info , State) ->
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
    logger:log(critical, "Handle continue"),
    {noreply , State}.

%%--------------------------------------------------------------------


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
                     % {ok , NewState :: #state{}} | {error , Reason :: term()}).
code_change(_OldVsn , State , _Extra) ->
    {ok , State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec(schedule_gc(Interval :: integer()) ->
    TimerRef :: reference() | ok).
schedule_gc(0) ->
    ok;
schedule_gc(Interval) ->
    TimerRef = erlang:send_after(Interval , ?SERVER , gc).

-spec(schedule_dets_sync(Interval :: pos_integer()) ->
    TimerRef :: reference() | ok).
schedule_dets_sync(Interval) ->
    case achlys_config:get(sync_dets_on_update, false) of
        true ->
            TimerRef = erlang:send_after(Interval , ?SERVER , dets_sync),
            logger:log(notice, "Syncing DETS"),
            TimerRef;
        _ ->
            ok
    end.
