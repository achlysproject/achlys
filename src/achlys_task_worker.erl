%%%-------------------------------------------------------------------
%%% @author Igor Kopestenski <igor.kopestenski@uclouvain.be>
%%%     [https://github.com/achlysproject/achlys]
%%% 2018, Universite Catholique de Louvain
%%% @doc
%%%
%%% @end
%%% Created : 05. Dec 2018 19:50
%%%-------------------------------------------------------------------
-module(achlys_task_worker).
-author("Igor Kopestenski <igor.kopestenski@uclouvain.be>").

-behaviour(gen_server).

-include("achlys.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1 ,
         handle_call/3 ,
         handle_cast/2 ,
         handle_info/2 ,
         terminate/2 ,
         code_change/3]).

%%====================================================================
%% Macros
%%====================================================================

-define(SERVER , ?MODULE).
% 65536 bytes of heap on 32-bit systems (4-byte words) i.e. 16384 words
% -define(MAX_HEAP_SIZE , (erlang:system_info(wordsize) * 1024 * 16)).
-define(MAX_HEAP_SIZE , 16384).

%%====================================================================
%% Records
%%====================================================================

-record(state , {
    tasks :: dict:dict(term(), term())
    , available_slots :: pos_integer()
    , task_lookup_interval :: pos_integer()
}).

-type state() :: state().

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
-spec(init(Args :: term()) ->
    {ok , State :: state()} | {ok , State :: state() , timeout() | hibernate} |
    {stop , Reason :: term()} | ignore).
init([]) ->
    logger:log(notice, "Initializing task worker module"),
    _Tab = achlys_util:create_table(task_history),

    Trigger = achlys_config:get(initial_task_lookup_delay, (?HMIN + ?MIN)) ,
    Interval = achlys_config:get(task_lookup_interval, (2 * ?MIN)) ,
    schedule_periodical_lookup(Trigger) ,

    {ok , #state{
        tasks = dict:new()
        , available_slots = 5
        , task_lookup_interval = Interval
    }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term() , From :: {pid() , Tag :: term()} ,
                  State :: state()) ->
                     {reply , Reply :: term() , NewState :: state()} |
                     {reply , Reply :: term() , NewState :: state() , timeout() | hibernate} |
                     {noreply , NewState :: state()} |
                     {noreply , NewState :: state() , timeout() | hibernate} |
                     {stop , Reason :: term() , Reply :: term() , NewState :: state()} |
                     {stop , Reason :: term() , NewState :: state()}).
handle_call(_Request , _From , State) ->
    {reply , ok , State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term() , State :: state()) ->
    {noreply , NewState :: state()} |
    {noreply , NewState :: state() , timeout() | hibernate} |
    {stop , Reason :: term() , NewState :: state()}).
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
-spec(handle_info(Info :: timeout() | term() , State :: state()) ->
    {noreply , NewState :: state()} |
    {noreply , NewState :: state() , timeout() | hibernate} |
    {stop , Reason :: term() , NewState :: state()}).
handle_info(periodical_lookup , State) ->
    Tasks = achlys_util:query(?TASKS),
    logger:log(notice, "Task list : ~p", [Tasks]),

    L = [ {H, spawn_task(T)} || {T, H} <- Tasks
        , permanent_execution(T, H) =:= true
        , dict:is_key(H, State#state.tasks) =:= false ],

    logger:log(notice, "New tasks : ~p", [L]),

    NewDict = lists:foldl(fun
        (Elem, AccIn) ->
            {H, Proc} = Elem,
            dict:store(H, Proc, AccIn)
    end, State#state.tasks, L),

    logger:log(notice, "New dict : ~p", [NewDict]),

    schedule_periodical_lookup(State#state.task_lookup_interval),

    {noreply , State#state{tasks = NewDict}, hibernate};

%%--------------------------------------------------------------------
handle_info({'DOWN', Ref, process, Pid, Info} , State) ->

    logger:log(notice, "Task finished : ~p", [Ref]),

    NewDict = dict:filter(fun
        (K, V) ->
            case V =/= {Pid, Ref} of
                true ->
                    true;
                false ->
                    case ets:insert_new(task_history, {K, V}) of
                        true  ->
                            false;
                        false ->
                            logger:log(warning, "ets:insert_new failed for entry : ~p", [{K, V}]),
                            false
                    end
            end
    end, State#state.tasks),

    true = erlang:demonitor(Ref, [flush]),

    logger:log(notice, "Task demonitored : ~p", [Ref]),
    logger:log(notice, "New dict : ~p", [NewDict]),
    logger:log(notice, "New Task history : ~p", [ets:match(task_history, '$1')]),

    {noreply , State#state{tasks = NewDict}};

%%--------------------------------------------------------------------
handle_info(_Info , State) ->
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
                State :: state()) -> term()).
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
-spec(code_change(OldVsn :: term() | {down , term()} , State :: state() ,
                  Extra :: term()) ->
                     {ok , NewState :: state()} | {error , Reason :: term()}).
code_change(_OldVsn , State , _Extra) ->
    {ok , State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Spawn a process that encapsulates a task function.
%%  
%% @spec spawn_task(Task) -> {Pid, Ref}
%% @end
-spec(spawn_task(Task :: achlys:task()) -> {Pid :: pid(), Ref :: reference()}).
spawn_task(Task) ->
    logger:log(notice, "Spawning task : ~p", [Task]),
    F = get_task_function(Task),
    logger:log(notice, "Func : ~p", [F]),

    {Pid, Ref} = erlang:spawn_opt(F, [monitor
        , {max_heap_size, #{size => 16384
            , kill => true
            , error_logger => false}}
        , {message_queue_data, off_heap}
        , {fullsweep_after, 0}]),

    logger:log(notice, "Spawned task ~p ", [Ref]),
    {Pid, Ref}.

%%--------------------------------------------------------------------
%% @private
get_task_function(Task) ->
    #{function := F} = Task,
    F.

%%--------------------------------------------------------------------
%% @private
permanent_execution(T, H) ->
    #{execution_type := ExecType} = T,
    case ExecType of
        ?PERMANENT_TASK ->
            true;
        ?SINGLE_EXECUTION_TASK ->
            not ets:member(task_history, H);
        _ ->
            false
    end.

%%--------------------------------------------------------------------
%% @private
schedule_periodical_lookup(Interval) ->
    erlang:send_after(Interval, ?SERVER, periodical_lookup).
