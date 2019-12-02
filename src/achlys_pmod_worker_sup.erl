%%%-------------------------------------------------------------------
%%% @author Igor Kopestenski <igor.kopestenski@uclouvain.be>
%%%     [https://github.com/achlysproject/achlys]
%%% 2018, Universite Catholique de Louvain
%%% @doc
%%%
%%% @end
%%% Created : 15. Feb 2019 01:02
%%%-------------------------------------------------------------------

-module(achlys_pmod_worker_sup).
-behaviour(supervisor).

-include("achlys.hrl").

%%====================================================================
%% API
%%====================================================================

-export([start_link/0]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

-export([init/1]).

%%====================================================================
%% Macros
%%====================================================================

-define(SERVER , ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

% {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
%% @doc Start achlys sensor worker supervisor.
-spec start_link() ->
    {ok , pid()}
    | {error , {already_started , pid()}
    | {shutdown , term()}
    | term()}.
start_link() ->
    supervisor:start_link({local , ?SERVER} , ?MODULE , []).


%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
%% @private
-spec init(term()) ->
    {ok , {supervisor:sup_flags() , [supervisor:child_spec()]}} 
    | ignore.
init([]) ->
    case Streamers = maps:keys(achlys_config:get(streamers, #{})) of
        [] ->
            ignore;
        _ when is_list(Streamers) ->
            StreamerSpecs = [ maps:get( S , ?STREAMERS) 
                || S <- Streamers,
                maps:is_key(S , ?STREAMERS ) ],
            {ok , { ?SUPFLAGS(2 , 6), StreamerSpecs }}
    end.



%%====================================================================
%% Internal functions
%%====================================================================

% -spec workers_specs(WorkersList :: [{atom(), boolean()}]) -> 
%     [supervisor:child_spec()] 
%     | [].
% workers_specs(WorkersList) ->
%     [ maps:get(K, ?WORKERS) 
%     || {K, true} <- WorkersList, maps:is_key(K, ?WORKERS)].

% L = workers_specs(maps:to_list(WorkersMap)),
% {ok , {
%     ?SUPFLAGS(3 , 10), lists:flatten(ChildSpecs)}}.
% StreamersMap = achlys_config:get(streamers, #{}),

% ChildSpecs = [ maps:get(K, ?STREAMERS) || K <- maps:keys(StreamersMap) ],
% ChildSpecs = [?CHILD(achlys_sensor_commander, worker)] ++ StreamerSpecs,
% ChildSpecs = StreamerSpecs,

% -spec start_worker(node()) -> {ok, pid()}.
% start_worker(Node) ->
%     supervisor:start_child(?MODULE, [Node]).
%
% -spec stop_worker(pid()) -> ok | {error, not_found}.
% stop_worker(Pid) ->
%     supervisor:terminate_child(?MODULE, Pid).