%%% ----------------------------------------------------------------------------
%%% @author <igor.kopestenski@uclouvain.be>
%%% @doc
%%%         achlys root supervisor implementation.
%%% @end
%%% @hidden

%% -----------------------------------------------------------------------------
-module(achlys_sup).
-author('igor.kopestenski@uclouvain.be').
-behaviour(supervisor).
%% -----------------------------------------------------------------------------
%% Exports:

%% 'application' API:
-export([start_link/0]).

%% 'supervisor' callback:
-export([init/1]).

%% Internal export:
-export(['$handle_undefined_function'/2]).

%% -----------------------------------------------------------------------------
%% Records & Macros & Includes:

-define(PROC, ?MODULE).

%% -----------------------------------------------------------------------------
%% API:

-spec
start_link() ->
    {ok, pid()} | {error, term()} | ignore.
start_link() ->
    supervisor:start_link({local, ?PROC}, ?MODULE, undefined).


%% -----------------------------------------------------------------------------
%% 'supervisor' callbacks:

%% @hidden
init(_) -> % (undefined)
    {ok, { {one_for_all, 0, 1}, []} }.


%% -----------------------------------------------------------------------------
%% Internal exports:

'$handle_undefined_function'(Func, [Arg]) ->
    case lists:member(Func, [delete_child
                            ,get_childspec
                            ,restart_child
                            ,start_child
                            ,terminate_child]) of
        true ->
            erlang:apply(supervisor, Func, [?PROC, Arg]);
        _ ->
            error_handler:raise_undef_exception(?MODULE, Func, [Arg])
    end;
'$handle_undefined_function'(Func, []) when Func == count_children orelse
                                            Func == which_children     ->
    erlang:apply(supervisor, Func, [?PROC]);
'$handle_undefined_function'(Func, Args) ->
    error_handler:raise_undef_exception(?MODULE, Func, Args).
