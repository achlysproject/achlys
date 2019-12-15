%%%-------------------------------------------------------------------
%%% @author Igor Kopestenski
%%% 2019, <UCLouvain>
%%% @doc
%%% Sample generic server demonstrating usage of the Achlys task
%%% model API.
%%% @end
%%%-------------------------------------------------------------------
-module(achlys_task_provider).
-author("Igor Kopestenski").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% Adds the pmodnav_task to the working set
%% using the Achlys task model
-export([add_pmodnav_task/0]).

%% gen_server callbacks
-export([init/1 ,
         handle_call/3 ,
         handle_cast/2 ,
         handle_info/2 ,
         terminate/2 ,
         code_change/3]).

-define(SERVER , ?MODULE).

-record(state , {}).

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

%%--------------------------------------------------------------------
%% @doc
%% Propagates the pmodnav_task
%% @end
%%--------------------------------------------------------------------
-spec(add_pmodnav_task() ->
    {ok , Pid :: pid()} | ignore | {error , Reason :: term()}).
add_pmodnav_task() ->
    gen_server:cast(?SERVER 
        , {task, pmodnav_task()}).

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
    {ok , State :: #state{}} | {ok , State :: #state{} , timeout() | hibernate} |
    {stop , Reason :: term()} | ignore).
init([]) ->
    ok = schedule_task(),
    logger:log(critical, "Running provider ~n"),
    {ok , #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term() , From :: {pid() , Tag :: term()} ,
                  State :: #state{}) ->
                     {reply , Reply :: term() , NewState :: #state{}} |
                     {reply , Reply :: term() , NewState :: #state{} , timeout() | hibernate} |
                     {noreply , NewState :: #state{}} |
                     {noreply , NewState :: #state{} , timeout() | hibernate} |
                     {stop , Reason :: term() , Reply :: term() , NewState :: #state{}} |
                     {stop , Reason :: term() , NewState :: #state{}}).
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
    {noreply , NewState :: #state{}} |
    {noreply , NewState :: #state{} , timeout() | hibernate} |
    {stop , Reason :: term() , NewState :: #state{}}).
handle_cast({task, Task} , State) ->
    logger:log(critical, "Received task cast signal ~n"),
    %% Task propagation to the cluster, including self
    achlys:bite(Task),    
    {noreply , State};
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
    {noreply , NewState :: #state{}} |
    {noreply , NewState :: #state{} , timeout() | hibernate} |
    {stop , Reason :: term() , NewState :: #state{}}).
handle_info({task, Task} , State) ->
    logger:log(critical, "Received task signal ~n"),
    %% Task propagation to the cluster, including self
    achlys:bite(Task),
    {noreply , State};
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
                     {ok , NewState :: #state{}} | {error , Reason :: term()}).
code_change(_OldVsn , State , _Extra) ->
    {ok , State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

schedule_task() ->
    %% Declare an Achlys task that will be
    %% executed exactly once
    Task = achlys:declare(mytask
        , all
        , single
        , fun() ->
             io:format("Hello Joe ! ~n")
    end),
    %% Send the task to the current server module
    %% after a 5000ms delay
    erlang:send_after(5000, ?SERVER, {task, Task}),
    ok.

%% https://github.com/grisp/grisp/wiki/PmodNAV-Tutorial
pmodnav_task() ->
    %% Declare an Achlys task that will be periodically
    %% executed as long as the node is up
    Task = achlys:declare(pmodnav_task
        , all
        , single
        , fun() ->
            logger:log(notice, "Reading PmodNAV measurements ~n"),
            Acc = pmod_nav:read(acc, [out_x_xl, out_y_xl, out_z_xl]),
            Gyro = pmod_nav:read(acc, [out_x_g, out_y_g, out_z_g]),
            Mag = pmod_nav:read(mag, [out_x_m, out_y_m, out_z_m]),
            Press = pmod_nav:read(alt, [press_out]),
            Temp = pmod_nav:read(alt, [temp_out]),
            Node = erlang:node(),

            F = fun({Acc, Gyro, Mag, Press, Temp, Node}) ->
                    [T] = Temp,
                    NewTemp = ((T * 1.8) + 32),
                    {Acc, Gyro, Mag, Press, [NewTemp], Node}
            end,
            {ok, {SourceId, _, _, _}} = lasp:declare({<<"source">>, state_orset}, state_orset),
            {ok, {DestinationId, _, _, _}} = lasp:declare({<<"destination">>, state_orset}, state_orset),
            lasp:map(SourceId, F, DestinationId),
            lasp:update(SourceId, {add, {Acc, Gyro, Mag, Press, Temp, Node}}, self())
    end).


%% Execution scenario
%% ==================
%%
%% Node 1 shell : 
%% --------------
%%
%% $ make shell n=1 PEER_PORT=27001
%% ...
%% booting up
%% ...
%%
%% (achlys1@130.104.213.164)1> achlys_task_provider:start_link().
%% {ok,<0.806.0>}
%% (achlys1@130.104.213.164)2> Hello Joe !
%% (achlys1@130.104.213.164)2> achlys_task_provider:add_pmodnav_task().
%% ok
%% (achlys1@130.104.213.164)3>
%% (achlys1@130.104.213.164)3> {ok, Set} = lasp:query({<<"source">>, state_orset}), sets:to_list(Set).
%% [{[-1.3732929999999999,-0.789584,-0.23198300000000002],
%%   [0.0,0.0,0.0],
%%   [0.0,0.0,0.0],
%%   [0.0],
%%   [42.5],
%%   'achlys1@130.104.213.164'}]
%% (achlys1@130.104.213.164)4>
%% (achlys1@130.104.213.164)4> {ok, FarenheitSet} = lasp:query({<<"destination">>, state_orset}), sets:to_list(FarenheitSet).
%% [{[-1.3732929999999999,-0.789584,-0.23198300000000002],
%%   [0.0,0.0,0.0],
%%   [0.0,0.0,0.0],
%%   [0.0],
%%   [108.5],
%%   'achlys1@130.104.213.164'}]
%% (achlys1@130.104.213.164)5>
%%
%% Now start a second Achlys shell : 
%%
%% Node 2 shell : 
%% --------------
%%
%% $ make shell n=2 PEER_PORT=27002
%% ...
%% booting up
%% ...
%%
%% (achlys2@130.104.213.164)1> achlys_util:add_node('achlys1@130.104.213.164').
%% ok
%% (achlys2@130.104.213.164)2> Hello Joe !
%% 
%% (achlys2@130.104.213.164)2>
%% (achlys2@130.104.213.164)2> {ok, FarenheitSet} = lasp:query({<<"destination">>, state_orset}), sets:to_list(FarenheitSet).
%% [{[-1.733376,-1.7716230000000002,0.24387799999999998],
%%   [0.0,0.0,0.0],
%%   [0.0,0.0,0.0],
%%   [0.0],
%%   [108.5],
%%   'achlys2@130.104.213.164'},
%%  {[-1.3732929999999999,-0.789584,-0.23198300000000002],
%%   [0.0,0.0,0.0],
%%   [0.0,0.0,0.0],
%%   [0.0],
%%   [108.5],
%%   'achlys1@130.104.213.164'}]
%% (achlys2@130.104.213.164)3>
%% (achlys2@130.104.213.164)3> achlys:get_all_tasks().
%% [{#{execution_type => <<1>>,
%%     function => #Fun<achlys_task_provider.0.44631258>,
%%     name => mytask,
%%     targets => <<0>>},
%%   128479609},
%%  {#{execution_type => <<1>>,
%%     function => #Fun<achlys_task_provider.1.44631258>,
%%     name => pmodnav_task,
%%     targets => <<0>>},
%%   30190207}]
%% (achlys2@130.104.213.164)4>