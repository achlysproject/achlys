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

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
%% @private
-spec init(term()) ->
    {ok , {supervisor:sup_flags() , [supervisor:child_spec()]}}.
init([]) ->
    {ok , {
        ?SUPFLAGS(5 , 25), [
            ?CHILD(achlys_squadron_leader, worker)
            , ?CHILD(achlys_cleaner, worker)
            , ?CHILD(achlys_task_server, worker)
            , ?CHILD(achlys_task_worker, worker)
            , ?CHILD(achlys_pmod_worker_sup, supervisor)
        ]}
    }.

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