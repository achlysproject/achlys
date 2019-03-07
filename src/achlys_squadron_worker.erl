%%%-------------------------------------------------------------------
%%% @author Igor Kopestenski <igor.kopestenski@uclouvain.be>
%%%     [https://github.com/Laymer/achlys]
%%% 2018, Universite Catholique de Louvain
%%% @doc
%%%
%%% @end
%%% Created : 15. Feb 2019 03:44
%%%-------------------------------------------------------------------
-module(achlys_squadron_worker).
-author("Igor Kopestenski <igor.kopestenski@uclouvain.be>").
%
% -behaviour(gen_server).
% -include("achlys.hrl").
%
% %% API
% -export([start_link/1]).
%
% %% gen_server callbacks
% -export([init/1 ,
%          handle_call/3 ,
%          handle_cast/2 ,
%          handle_info/2 ,
%          terminate/2 ,
%          code_change/3]).
%
% %%====================================================================
% %% Macros
% %%====================================================================
%
% -define(SERVER , ?MODULE).
%
% %%====================================================================
% %% Records
% %%====================================================================
%
% %%%===================================================================
% %%% API
% %%%===================================================================
%
% %%--------------------------------------------------------------------
% %% @doc
% %% Starts the server
% %%
% %% @end
% %%--------------------------------------------------------------------
% -spec start_link(node()) -> {ok, pid()}.
% start_link(Node) ->
% 	gen_server:start_link(?MODULE, Node, []).
%
%
% init(Node) ->
%     ok = net_kernel:monitor_nodes(true),
%     {ok, init_timer(#state{node = Node, node_up = false}, 0)}.
%
% %%%===================================================================
% %%% gen_server callbacks
% %%%===================================================================
% -module(erlang_node_discovery_worker).
% -behaviour(gen_server).
%
% %% API.
% -export([start_link/1]).
%
% %% gen_server.
% -export([init/1]).
% -export([handle_call/3]).
% -export([handle_cast/2]).
% -export([handle_info/2]).
% -export([terminate/2]).
% -export([code_change/3]).
%
%
% -record(state, {
%     node    :: node(),
%     node_up :: boolean(),
%     timer   :: reference()
% }).
%
% -type state() :: #state{}.
%
%
% -spec start_link(node()) -> {ok, pid()}.
% start_link(Node) ->
% 	gen_server:start_link(?MODULE, Node, []).
%
%
% init(Node) ->
%     % ok = net_kernel:monitor_nodes(true),
%     % {ok, init_timer(#state{node = Node, node_up = false}, 0)}.
%     {ok, #state{}}.
%
%
% handle_call(Msg, _From, State) ->
%     error_logger:error_msg("Unexpected message: ~p~n", [Msg]),
% 	{reply, {error, {bad_msg, Msg}}, State}.
%
%
% handle_cast(Msg, State) ->
%     error_logger:error_msg("Unexpected message: ~p~n", [Msg]),
% 	{noreply, State}.
%
%
% handle_info({nodeup, Node}, State = #state{node = Node}) ->
%     {noreply, State#state{node_up = true}};
%
% handle_info({nodeup, _}, State) ->
%     {noreply, State};
%
% handle_info({nodedown, Node}, State = #state{node = Node}) ->
%     {noreply, init_timer(State#state{node_up = false})};
%
% handle_info({nodedown, _}, State) ->
%     {noreply, State};
%
% handle_info(ping, State = #state{node_up = true}) ->
%     {noreply, State};
% handle_info(ping, State = #state{node = Node}) ->
%     _ = net_adm:ping(Node),
%     {noreply, init_timer(State)};
%
% handle_info(Msg, State) ->
%     error_logger:error_msg("Unexpected message: ~p~n", [Msg]),
% 	{noreply, State}.
%
%
% terminate(_Reason, _State) ->
% 	ok.
%
%
% code_change(_OldVsn, State, _Extra) ->
% 	{ok, State}.
%
%
% %% internal funcs
%
%
% -spec init_timer(State) -> NewState when
%       State    :: #state{},
%       NewState :: #state{}.
% init_timer(State) ->
%     init_timer(State, 30000).
%
%
% -spec init_timer(State, Delay) -> NewState when
%       State    :: #state{},
%       Delay    :: non_neg_integer(),
%       NewState :: #state{}.
% init_timer(State = #state{timer = Timer}, Delay) ->
%     catch erlang:cancel_timer(Timer),
%     State#state{timer = erlang:send_after(Delay, self(), ping)}.
%
% %%--------------------------------------------------------------------
% %% @private
% %% @doc
% %% Initializes the server
% %%
% %% @spec init(Args) -> {ok, State} |
% %%                     {ok, State, Timeout} |
% %%                     ignore |
% %%                     {stop, Reason}
% %% @end
% %%--------------------------------------------------------------------
% -spec(init(Args :: term()) ->
%     {ok , State :: #state{}} | {ok , State :: #state{} , timeout() | hibernate} |
%     {stop , Reason :: term()} | ignore).
% init([]) ->
%     logger:log(notice , "Initializing cluster maintainer. ~n") ,
%     Trigger = achlys_config:get(initial_formation_delay, ?HMIN) ,
%     Interval = achlys_config:get(formation_check_interval, ?MIN) ,
%     schedule_formation(Trigger) ,
%     {ok , #state{
%         % initial_formation_delay = Trigger ,
%         formation_check_interval = Interval
%     }}.
%
% %%--------------------------------------------------------------------
% %% @private
% %% @doc
% %% Handling call messages
% %%
% %% @end
% %%--------------------------------------------------------------------
% -spec(handle_call(Request :: term() , From :: {pid() , Tag :: term()} ,
%                   State :: #state{}) ->
%                      {reply , Reply :: term() , NewState :: #state{}} |
%                      {reply , Reply :: term() , NewState :: #state{} , timeout() | hibernate} |
%                      {noreply , NewState :: #state{}} |
%                      {noreply , NewState :: #state{} , timeout() | hibernate} |
%                      {stop , Reason :: term() , Reply :: term() , NewState :: #state{}} |
%                      {stop , Reason :: term() , NewState :: #state{}}).
% handle_call(_Request , _From , State) ->
%     {reply , ok , State}.
%
% %%--------------------------------------------------------------------
% %% @private
% %% @doc
% %% Handling cast messages
% %%
% %% @end
% %%--------------------------------------------------------------------
% -spec(handle_cast(Request :: term() , State :: #state{}) ->
%     {noreply , NewState :: #state{}} |
%     {noreply , NewState :: #state{} , timeout() | hibernate} |
%     {stop , Reason :: term() , NewState :: #state{}}).
% handle_cast(_Request , State) ->
%     {noreply , State}.
%
% %%--------------------------------------------------------------------
% %% @private
% %% @doc
% %% Handling all non call/cast messages
% %%
% %% @spec handle_info(Info, State) -> {noreply, State} |
% %%                                   {noreply, State, Timeout} |
% %%                                   {stop, Reason, State}
% %% @end
% %%--------------------------------------------------------------------
% -spec(handle_info(Info :: timeout() | term() , State :: #state{}) ->
%     {noreply , NewState :: #state{}} |
%     {noreply , NewState :: #state{} , timeout() | hibernate} |
%     {stop , Reason :: term() , NewState :: #state{}}).
% handle_info(formation , State) ->
%     % _ = achlys:clusterize() ,
%     % _ = achlys:contagion() ,
%     % maybe_clusterize(),
%     maybe_concurrent_clusterize(),
%     _ = schedule_formation(State#state.formation_check_interval) ,
%     % erlang:send_after(?MIN , ?SERVER , formation) ,
%     {noreply , State, hibernate};
%
% handle_info(_Info , State) ->
%     {noreply , State}.
%
% %%--------------------------------------------------------------------
% %% @private
% %% @doc
% %% This function is called by a gen_server when it is about to
% %% terminate. It should be the opposite of Module:init/1 and do any
% %% necessary cleaning up. When it returns, the gen_server terminates
% %% with Reason. The return value is ignored.
% %%
% %% @spec terminate(Reason, State) -> void()
% %% @end
% %%--------------------------------------------------------------------
% -spec(terminate(Reason :: (normal | shutdown | {shutdown , term()} | term()) ,
%                 State :: #state{}) -> term()).
% terminate(_Reason , _State) ->
%     ok.
%
% %%--------------------------------------------------------------------
% %% @private
% %% @doc
% %% Convert process state when code is changed
% %%
% %% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
% %% @end
% %%--------------------------------------------------------------------
% -spec(code_change(OldVsn :: term() | {down , term()} , State :: #state{} ,
%                   Extra :: term()) ->
%                      {ok , NewState :: #state{}} | {error , Reason :: term()}).
% code_change(_OldVsn , State , _Extra) ->
%     {ok , State}.
%
% %%%===================================================================
% %%% Internal functions
% %%%===================================================================
%
% %% private
% schedule_formation(Interval) ->
%     erlang:send_after(Interval , ?SERVER , formation).
%
% maybe_clusterize() ->
%     Reached = [ lasp_peer_service:join(X) || X <- [ achlys@my_grisp_board_1 ,
%         achlys@my_grisp_board_2 ,
%         achlys@my_grisp_board_3 ,
%         achlys@my_grisp_board_4 ,
%         achlys@my_grisp_board_5 ,
%         achlys@my_grisp_board_6 ,
%         achlys@my_grisp_board_7 ,
%         achlys@my_grisp_board_8 ,
%         achlys@my_grisp_board_9 ,
%         achlys@my_grisp_board_10 ]] ,
%     logger:log(notice, "Formation result : ~p ~n ", [Reached]).
%
% maybe_concurrent_clusterize() ->
%     % Reached = [ spawn(fun() -> maybe_reach(X) end) || X <- ?BOARDS ] ,
%     Reached = [ spawn(fun() -> maybe_reach(X) end) || X <- [ achlys@my_grisp_board_1 ,
%         achlys@my_grisp_board_2 ,
%         achlys@my_grisp_board_3 ,
%         achlys@my_grisp_board_4 ,
%         achlys@my_grisp_board_5 ,
%         achlys@my_grisp_board_6 ,
%         achlys@my_grisp_board_7 ,
%         achlys@my_grisp_board_8 ,
%         achlys@my_grisp_board_9 ,
%         achlys@my_grisp_board_10 ] ] ,
%     logger:log(notice, "Formation result : ~p ~n ", [lasp_peer_service:members()]).
%
% maybe_reach(Node) ->
%     % lists:mapfoldl(fun(X, Reached) ->
%     %     case expression of
%     %         pattern when guard ->
%     %             body
%     %     end
%     %     % {2*X, X+Reached} end,
%     %     0, [1,2,3,4,5]).
%     case net_kernel:hidden_connect_node(Node) of
%         true ->
%             ok = lasp_peer_service:join(Node),
%             true = schedule_disterl_disconnect(Node),
%             {true, Node};
%         false ->
%             logger:log(notice, "net_kernel:hidden_connect_node(Node) failed for ~p ~n ", [Node]),
%             {false, Node};
%         ignored ->
%             logger:log(notice, "net_kernel:hidden_connect_node(Node) ignored for ~p ~n ", [Node]),
%             {false, Node};
%         _ ->
%             logger:log(notice, "net_kernel:hidden_connect_node(Node) undef for ~p ~n ", [Node]),
%             {false, Node}
%     end.
%
% schedule_disterl_disconnect(Node) ->
%     true = net_kernel:disconnect(Node).
