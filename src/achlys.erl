%%%-------------------------------------------------------------------
%%% @author Igor Kopestenski <igor.kopestenski@uclouvain.be>
%%%     [https://github.com/Laymer/achlys]
%%% @doc
%%% Top level module operating the achlys application
%%% as well as providing the main API to the underlying components.
%%% @end
%%% Created : 06. Nov 2018 20:38
%%%-------------------------------------------------------------------
-module(achlys).
-author("Igor Kopestenski <igor.kopestenski@uclouvain.be>").

-include("achlys.hrl").
-include_lib("partisan/include/partisan.hrl").

%% Application control
-export([start/0]).
-export([stop/0]).

%% Task model API
-export([get_all_tasks/0]).
-export([bite/1]).
-export([declare/4]).

%% API
-export([clusterize/0]).
-export([contagion/0]).
-export([pandemia/0]).
-export([get_preys/0]).
% -export([get_bounded_preys/0]).
-export([bidirectional_join/1]).

%% PMOD-related functions API
-export([bane/1]).
-export([bane_all_preys/1]).
-export([venom/0]).
-export([venom/1]).

%% Shortcuts
-export([members/0]).
-export([gc/0]).
-export([flush/1]).
-export([join_host/1]).

-export([rainbow/0]).
-export([light/0]).
-export([mintemp/0]).

%%====================================================================
%% Type definitions
%%====================================================================

%%====================================================================
%% Macros
%%====================================================================

% -define(MANAGER,    lasp_peer_service:manager()).
% -define(MANAGER,    partisan_hyparview_peer_service_manager).
-define(MANAGER,    partisan_default_peer_service_manager).
-define(LPS,    lasp_peer_service).

%% ===================================================================
%% Entry point functions
%% ===================================================================

%% @doc Start the application.
-spec start() -> ok.
start() ->
    {ok , _} = application:ensure_all_started(achlys) ,
    ok.

%% @doc Stop the Application
-spec stop() -> ok.
stop() ->
    ok = application:stop(achlys) ,
    ok.

%% ===================================================================
%% API
%% ===================================================================

%% @doc Returns current view of all tasks.
-spec get_all_tasks() -> [achlys:task()] | [].
get_all_tasks() ->
    {ok, Set} = lasp:query(?TASKS),
    sets:to_list(Set).

%% @doc Shortcut function exposing the utility function that can be
%% used to pass more readable arguments to create a task model variable
%% instead of binary strings. The ExecType argument can currently not
%% provide a transient mode, a task is executed either once or permanently.
%% Removing the task from the CRDT does not prevent nodes to keep executing it.
%% But similar behavior can be created with a single execution task that
%% specifies a function with several loops. However this does not allow the
%% worker to have full control over the load, since backpressure can be applied
%% by spawning a process that executes a permanent task at a controlled
%% frequency that can be based on any stress parameter. Meanwhile a single
%% execution task could possibly overload the worker, as iterations are
%% embedded inside a single process.
%%
%% When Grow-only Counters and Sets are used, the transient can be achieved
%% using Lasp's monotonic read function to have a distributed treshold.
%%
%%
-spec declare(Name::atom()
    , Targets::[node()] | all
    , ExecType::single | permanent
    , Func::function()) -> task() | erlang:exception().
declare(Name, Targets, ExecType, Func) ->
    achlys_util:declare(Name, Targets, ExecType, Func).
    % {ok, Set} = lasp:query(?TASKS),
    % sets:to_list(Set).

-spec rainbow() -> erlang:function().
rainbow() ->
    achlys_util:rainbow().

-spec light() -> erlang:function().
light() ->
    ok.

-spec mintemp() -> erlang:function().
mintemp() ->
    F = fun() ->
        Id = {<<"temp">>, state_gset},
        {ok, {_, _, _, _}} = lasp:declare(Id, state_gset),
        % lists:foldl(fun(X, Prod) -> X * Prod end, 1, [1,2,3,4,5]).
        L = lists:foldl(fun
            (Elem, AccIn) ->
                timer:sleep(5000),
                Temp = pmod_nav:read(acc, [out_temp]),
                Temp ++ AccIn
        end, [], lists:seq(1,5)),
        SList = lists:usort(L),

        Min = hd(SList),
        Name = node(),

        lasp:update(Id, {add, {Min, Name}}, self()),

        spawn(fun
            () ->
                lasp:read(Id, {cardinality, 5}),
                {ok, S} = lasp:query(Id),
                Fetched = sets:to_list(S),
                {Minimum, Node} = hd(lists:usort(Fetched)),
                Self = node(),
                case Node =:= Self of
                    true ->
                        grisp_led:color(1, blue),
                        grisp_led:color(2, blue);
                    _ ->
                        grisp_led:color(1, red),
                        grisp_led:color(2, red)
                end
        end)
    end.

%% @doc Adds the given task in the replicated task set.
%% @see achlys_task_server:add_task/1.
-spec bite(Task :: achlys:task()) -> ok.
bite(Task) ->
    achlys_task_server:add_task(Task).

%% @doc Attempts to discover and join other neighboring nodes.
-spec clusterize() -> [atom()].
clusterize() ->
    logger:log(notice , "Cluster formation attempt ~n") ,
    N = seek_neighbors() ,
    Remotes = binary_remotes_to_atoms(N) ,
    Reachable = [R || R <- Remotes
        ,        R =/= node()
        ,        net_adm:ping(R) =:= pong] ,
    clusterize(Reachable).

%% @doc Form Lasp cluster without attempting to ping neighbors beforehand.
-spec contagion() -> [{ error , atom() } | { ok , atom() } ].
contagion() ->
    logger:log(notice , "Pure Lasp Cluster formation attempt ~n") ,
    % L = get_preys(),
    % Self = ?MANAGER:myself(),
    N = seek_neighbors() ,
    Remotes = binary_remotes_to_atoms(N) ,
    [ bidirectional_join(R) || R <- Remotes
        ,        R =/= node()].

%% @doc Close disterl TCP connections with neighboring nodes.
-spec pandemia() -> ok.
pandemia() ->
    logger:log(notice , "Closing native Erlang connections ~n") ,
    _ = [ net_kernel:disconnect(X) || X <- get_preys() ],
    ok.

%% @doc Returns the aggregates for the given variable
%% as seen by the current node.
-spec bane(atom()) -> list().
% bane(Data) when is_atom(Data) ->
bane(Data) ->
    logger:log(notice , "Reading ~p CRDT ~n", [Data]) ,
    % Id = {atom_to_binary(Data, utf8), state_awset_ps},
    Id = {atom_to_binary(Data, utf8), state_awset},
    % L = [ unicode:characters_to_binary([X
    %     , "_"
    %     , erlang:atom_to_binary(Data,utf8)] , utf8)
    %     || X <- seek_neighbors() ],
    {ok, S} = lasp:query(Id),
    sets:to_list(S).

%% @doc Returns the aggregates for the given variable
%% as seen by the current node for all nodes.
-spec bane_all_preys(atom()) -> list().
bane_all_preys(temperature) ->
    logger:log(notice , "Reading temperature CRDT ~n", []) ,
    lists:flatten([ achlys_util:query(Id) || Id <- ?TEMP_LIST ]);
bane_all_preys(pressure) ->
    logger:log(notice , "Reading pressure CRDT ~n", []) ,
    lists:flatten([ achlys_util:query(Id) || Id <- ?PRESS_LIST ]);
bane_all_preys(Data) ->
    logger:log(error , "Unknown value ~p ~n", [Data]) ,
    [undefined].
    % bane(L).
% bane([H|T]) ->
%     {ok, S} = lasp:query({H, state_awset}),
%     L = sets:to_list(S),
%     [ L | bane(T) ];
% bane([]) ->
%     [].



%% @doc Collect data based on sensors available on Pmod modules and store
%% aggregated values in corresponding Lasp variable.
-spec venom() -> ok.
venom() ->
    achlys_pmod_nav_worker:run().

%% @doc Collect data based on sensors available on Pmod modules and store
%% aggregated values in corresponding Lasp variable.
-spec venom(atom()) -> ok.
venom(Worker) ->
    Worker:run().
    % achlys_pmod_nav_worker:run().

%%====================================================================
%% Clustering helper functions
%%====================================================================

%% @doc Returns a list of known remote hostnames
%% that could be potential neighbors.
get_preys() ->
    binary_remotes_to_atoms(seek_neighbors()).

%% @private
binary_remotes_to_atoms([H | T]) ->
    [binary_to_atom(H , utf8) | binary_remotes_to_atoms(T)];
binary_remotes_to_atoms([]) ->
    [].

%% @private
seek_neighbors() ->
    Rc = inet_db:get_rc() ,
    seek_neighbors(Rc).
seek_neighbors([{host , _Addr , N} | T]) ->
    [list_to_bitstring(["achlys@" , N]) | seek_neighbors(T)];
seek_neighbors([{_Arg , _Val} | T]) ->
    seek_neighbors(T);
seek_neighbors([]) ->
    [].

%% @private
join(Host) ->
    % try rpc:call(Host , partisan_hyparview_peer_service_manager , myself , []) of
    try rpc:call(Host , partisan_default_peer_service_manager , myself , []) of
        #{channels := _Channels
        , listen_addrs := _Addresses
        , name := _Name
        , parallelism := _Parallelism } = Node ->
            ok = lasp_peer_service:join(Node),
            {ok, Host}
    catch
        {badrpc, Reason} ->
            logger:log(error , "Unable to RPC remote : ~p~n" , [Reason]) ,
            {error , Reason};
        {error, Reason} ->
            logger:log(error , "Unable to retrieve remote : ~p~n" , [Reason]) ,
            {error , Reason}
    end.

%% @private
bidirectional_join(Host) ->
    Node = #{channels := _Channels
    , listen_addrs := _Addresses
    , name := _Name
    , parallelism := _Parallelism } = try rpc:call(Host , partisan_hyparview_peer_service_manager , myself , [])
    catch
        error:{badrpc, nodedown} ->
            logger:log(error , "Unable to RPC remote : Node down~n") ,
            {error, nodedown};
        error:{badrpc, Reason} ->
            logger:log(error , "Unable to RPC remote : ~p~n" , [Reason]) ,
            {error , Reason};
        _:Reason ->
            logger:log(error , "Unable to retrieve remote : ~p~n" , [Reason]) ,
            {error , Reason}
    end,
    lasp_peer_service:join(Node) ,
    ok = try rpc:call(Host,lasp_peer_service,join,[partisan_hyparview_peer_service_manager:myself()])
    catch
        error:{badrpc, nodedown} ->
            logger:log(error , "Unable to RPC remote : Node down~n") ,
            {error, nodedown};
        error:{badrpc, RemoteReason} ->
            logger:log(error , "Unable to RPC remote : ~p~n" , [RemoteReason]) ,
            {error , RemoteReason};
        _:RemoteReason ->
            logger:log(error , "Unable to retrieve remote : ~p~n" , [RemoteReason]) ,
            {error , RemoteReason}
    end,
    Node.

%% @private
clusterize([H | Remotes]) ->
    case join(H) of
        {ok , N} ->
            [N | clusterize(Remotes)];
        _ ->
            logger:log(warning , "Failed to join : ~p ~n" , [H]) ,
            clusterize(Remotes)
    end;

clusterize([]) ->
    [].

%% @private
members() ->
    ?LPS:members().

%% @private
gc() ->
    achlys_util:do_gc().

%% @private
flush(Name) ->
    BS = atom_to_binary(Name , utf8),
    lasp:update({BS,state_awset}
        , {rmv_all, achlys_util:query({BS,state_awset})}
        , self()).

%% @private
join_host(Host) ->
    lasp_peer_service:join(Host).
