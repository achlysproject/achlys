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

%% Pmod_NAV related functions API
-export([aggregate_sensor_data/0]).
-export([get_temp/0]).


%%====================================================================
%% Type definitions
%%====================================================================

%% TODO : aggregation parameters must be more generic and should provide
%% configuration possibilities that are loosely coupled, even completely
%% independent from each other if possible e.g. several temperature aggregation
%% configurations that do not interfere.
%% Configuration parameters
%% -type data_config()        :: {atom(),  #{table := atom()
%%                                     , device := atom()
%%                                     , poll_interval := pos_integer()
%%                                     , aggregation_trigger := pos_integer() }}.

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
-spec clusterize() -> [node_spec()].
clusterize() ->
    logger:log(notice , "Cluster formation attempt ~n") ,
    N = seek_neighbors() ,
    Remotes = binary_remotes_to_atoms(N) ,
    Reachable = [R || R <- Remotes
        ,        R =/= node()
        ,        net_adm:ping(R) =:= pong] ,
    clusterize(Reachable).

%% @doc Returns the temperature aggregates seen by the current node.
-spec get_temp() -> list().
get_temp() ->
    logger:log(notice , "Reading temp CRDT ~n") ,
    achlys_pmod_nav_worker:get_crdt().

% TODO : add genericity by specifying the desired aggregates in the
% data_config() argument, and start corresponding pmod worker as a child
% in the supervision tree if not yet present. Add the value to the polling
% process otherwise.
% -spec aggregate_sensor_data(data_config()) -> ok | {error, Reason}.
%
%% @doc Collect data based on sensors available on Pmod modules and store
%% aggregated values in corresponding Lasp variable.
-spec aggregate_sensor_data() -> ok.
aggregate_sensor_data() ->
    achlys_pmod_nav_worker:run().


%%====================================================================
%% Clustering helper functions
%%====================================================================

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
    Manager = rpc:call(Host , partisan_peer_service , manager , []) ,
    case Manager of
        partisan_hyparview_peer_service_manager ->
            Node = rpc:call(Host , Manager , myself , []) ,
            ok = partisan_peer_service:join(Node) ,
            logger:log(info , "Joined ~p~n" , [Host]) ,
            {ok , Node};
        {badrpc , Reason} ->
            logger:log(error , "Unable to RPC remote : ~p~n" , [Reason]) ,
            {error , Reason};
        {error , Reason} ->
            logger:log(error , "Unable to retrieve remote : ~p~n" , [Manager]) ,
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
