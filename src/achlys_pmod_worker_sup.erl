%%%-------------------------------------------------------------------
%%% @author Igor Kopestenski <igor.kopestenski@uclouvain.be>
%%%     [https://github.com/Laymer/achlys]
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
% -export([start_worker/1]).
% -export([stop_worker/1]).

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
%% @doc Start achlys top level supervisor.
-spec start_link() ->
    {ok , pid()}
    | {error , {already_started , pid()}
    | {shutdown , term()}
    | term()}.
start_link() ->
    supervisor:start_link({local , ?SERVER} , ?MODULE , []).

% -spec start_worker(node()) -> {ok, pid()}.
% start_worker(Node) ->
%     supervisor:start_child(?MODULE, [Node]).
%
% -spec stop_worker(pid()) -> ok | {error, not_found}.
% stop_worker(Pid) ->
%     supervisor:terminate_child(?MODULE, Pid).

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

    {ok , {
        ?SUPFLAGS(?THREE , ?TEN)
        , lists:flatten(WorkersSpecs)}}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
-spec workers_specs(WorkersList :: [{atom(), boolean()}]) ->
    [supervisor:child_spec()] | [].
workers_specs(WorkersList) ->
    [ maps:get(K, ?WORKERS) || {K, true} <- WorkersList
                            , maps:is_key(K, ?WORKERS)].
