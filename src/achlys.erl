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

%% API
-export([clusterize/0]).
-export([contagion/0]).
-export([pandemia/0]).
-export([get_preys/0]).

%% Pmod_NAV related functions API
-export([venom/0]).
-export([venom/1]).
-export([bane/1]).

%%====================================================================
%% Type definitions
%%====================================================================

%%====================================================================
%% Macros
%%====================================================================

-define(MANAGER,    lasp_peer_service:manager()).

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

%% @doc Returns the aggregates for the given variable
%% as seen by the current node.
-spec bane(atom()) -> list().
bane(Data) ->
    logger:log(notice , "Reading ~p CRDT ~n", [Data]) ,
    % Id = {atom_to_binary(Data, utf8), state_awset_ps},
    Id = {atom_to_binary(Data, utf8), state_awset},
    {ok, S} = lasp:query(Id),
    sets:to_list(S).

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

-spec contagion() -> list().
contagion() ->
    logger:log(notice , "Pure Lasp Cluster formation attempt ~n") ,
    [ lasp_peer_service:join(X) || X <- get_preys() ].

-spec pandemia() -> list().
pandemia() ->
    logger:log(notice , "Closing native Erlang connections ~n") ,
    _ = [ net_kernel:disconnect(X) || X <- get_preys() ],
    ok.

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

% %% @private
% join(Host) ->
%     Manager = rpc:call(Host , partisan_peer_service , manager , []) ,
%     case Manager of
%         partisan_hyparview_peer_service_manager ->
%             Node = rpc:call(Host , Manager , myself , []) ,
%             ok = partisan_peer_service:join(Node) ,
%             logger:log(info , "Joined ~p~n" , [Host]) ,
%             {ok , Node};
%         {badrpc , Reason} ->
%             logger:log(error , "Unable to RPC remote : ~p~n" , [Reason]) ,
%             {error , Reason};
%         {error , Reason} ->
%             logger:log(error , "Unable to retrieve remote : ~p~n" , [Manager]) ,
%             {error , Reason}
%     end.
%% @private
join(Host) ->
    try rpc:call(Host , lasp_peer_service:manager() , myself , []) of
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
