%%%-------------------------------------------------------------------
%% @doc achlys top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(achlys_sup).
-author("Igor Kopestenski <igor.kopestenski@uclouvain.be>").

-behaviour(supervisor).

-include("achlys.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER , ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

% {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
%% @doc Start achlys top level supervisor.
-spec start_link() ->
    {ok , pid()} | {error , {already_started , pid()} | {shutdown , term()} | term()}.
start_link() ->
    supervisor:start_link({local , ?SERVER} , ?MODULE , []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
%% @private
-spec init(term()) ->
    {ok , {supervisor:sup_flags() , [supervisor:child_spec()]}}.
init([]) ->

    {ok, WorkersMap} = achlys_config:get(workers),

    WorkersSpecs = case erlang:is_map(WorkersMap) of
        true ->
            workers_specs(maps:to_list(WorkersMap));
        _ ->
            []
    end,

    ChildSpecs = [?TASK_SERVER, ?TASK_WORKER, ?LOAD_GENERATOR],

    {ok , {?SUPFLAGS(?THREE , ?TEN) , lists:flatten(WorkersSpecs ++ ChildSpecs)}}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
-spec workers_specs(WorkersList :: [{atom(), boolean()}]) -> [supervisor:child_spec()] | [].
workers_specs(WorkersList) ->
    [ maps:get(K, ?WORKERS) || {K, true} <- WorkersList, maps:is_key(K, ?WORKERS)].
