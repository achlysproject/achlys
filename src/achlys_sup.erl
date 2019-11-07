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

%% Internal export:
-export(['$handle_undefined_function'/2]).

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

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
%% @private
-spec init(term()) ->
    {ok , {supervisor:sup_flags() , [supervisor:child_spec()]}}.
init([]) ->

    WorkersMap = achlys_config:get(workers, #{}),
    L = workers_specs(maps:to_list(WorkersMap)),

    ChildSpecs = L ++ [
        ?TASK_SERVER
        , ?TASK_WORKER
        , ?PMOD_WORKER_SUPERVISOR
    ],

    {ok , {
        ?SUPFLAGS(3 , 10), lists:flatten(ChildSpecs)}}.

%%====================================================================
%% Internal functions
%%====================================================================

'$handle_undefined_function'(Func, [Arg]) ->
    case lists:member(Func, [delete_child
                            , get_childspec
                            , restart_child
                            , start_child
                            , terminate_child]) of
        true ->
            erlang:apply(supervisor, Func, [?SERVER, Arg]);
        _ ->
            error_handler:raise_undef_exception(?MODULE, Func, [Arg])
    end;

'$handle_undefined_function'(Func, []) when Func == count_children orelse
                                            Func == which_children     ->
    erlang:apply(supervisor, Func, [?SERVER]);

'$handle_undefined_function'(Func, Args) ->
    error_handler:raise_undef_exception(?MODULE, Func, Args).

%% @private
-spec workers_specs(WorkersList :: [{atom(), boolean()}]) -> [supervisor:child_spec()] | [].
workers_specs(WorkersList) ->
    [ maps:get(K, ?WORKERS) || {K, true} <- WorkersList, maps:is_key(K, ?WORKERS)].
