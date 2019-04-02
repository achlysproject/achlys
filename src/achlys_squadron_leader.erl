%%%-------------------------------------------------------------------
%%% @author Igor Kopestenski <igor.kopestenski@uclouvain.be>
%%%     [https://github.com/Laymer/achlys]
%%% 2018, Universite Catholique de Louvain
%%% @doc
%%%
%%% @end
%%% Created : 13. Dec 2018 19:30
%%%-------------------------------------------------------------------
-module(achlys_squadron_leader).
-author("Igor Kopestenski <igor.kopestenski@uclouvain.be>").

-behaviour(gen_server).

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

%%====================================================================
%% Records
%%====================================================================

-record(state , {
    % initial_formation_delay :: pos_integer() ,
    formation_check_interval :: pos_integer(),
    boards :: list()
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
    logger:log(notice , "Initializing cluster maintainer. ~n") ,
    Trigger = achlys_config:get(initial_formation_delay, 30000) ,
    Interval = achlys_config:get(formation_check_interval, 60000) ,
    Boards = achlys_config:get(boards, []) ,
    schedule_formation(Trigger) ,
    {ok , #state{
        % initial_formation_delay = Trigger ,
        formation_check_interval = Interval,
        boards = Boards
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
handle_info(formation , State) ->
    % _ = achlys:clusterize() ,
    % _ = achlys:contagion() ,
    % maybe_clusterize(),
    maybe_concurrent_clusterize(State#state.boards),
    _ = schedule_formation(State#state.formation_check_interval) ,
    % erlang:send_after(?MIN , ?SERVER , formation) ,
    {noreply , State, hibernate};
handle_info({disconnect_disterl, Node} , State) ->
    true = net_kernel:disconnect(Node),
    logger:log(notice, "Disterl connection closed for node : ~p ~n ", [Node]),
    {noreply , State, hibernate};

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

%% private
schedule_formation(Interval) ->
    erlang:send_after(Interval , ?SERVER , formation).

maybe_clusterize(Boards) ->
    Reached = [ lasp_peer_service:join(X) || X <- Boards] ,
    logger:log(notice, "Formation result : ~p ~n ", [Reached]).

maybe_concurrent_clusterize(Boards) ->
    % Reached = [ spawn(fun() -> maybe_reach(X) end) || X <- ?BOARDS ] ,
    Reached = [ spawn(fun() -> maybe_reach(X) end) || X <-  Boards ] ,
    logger:log(notice, "Formation result : ~p ~n ", [lasp_peer_service:members()]),
    logger:log(notice, "Formation PIDs : ~p ~n ", [Reached]).

maybe_reach(Node) ->
    % lists:mapfoldl(fun(X, Reached) ->
    %     case expression of
    %         pattern when guard ->
    %             body
    %     end
    %     % {2*X, X+Reached} end,
    %     0, [1,2,3,4,5]).
    case net_kernel:hidden_connect_node(Node) of
        true ->
            ok = lasp_peer_service:join(Node),
            true = schedule_disterl_disconnect(Node),
            {true, Node};
        false ->
            logger:log(notice, "net_kernel:hidden_connect_node(Node) failed for ~p ~n ", [Node]),
            {false, Node};
        ignored ->
            logger:log(notice, "net_kernel:hidden_connect_node(Node) ignored for ~p ~n ", [Node]),
            {false, Node};
        _ ->
            logger:log(notice, "net_kernel:hidden_connect_node(Node) undef for ~p ~n ", [Node]),
            {false, Node}
    end.

schedule_disterl_disconnect(Node) ->
    % true = net_kernel:disconnect(Node).
    erlang:send_after(60000 , ?SERVER , {disconnect_disterl, Node}).
