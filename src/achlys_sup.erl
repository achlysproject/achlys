%%%-------------------------------------------------------------------
%% @doc achlys top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(achlys_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Thanks to https://github.com/erszcz
%% Helper macro for declaring children of supervisor
% -define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(SUPFLAGS(Intensity, Period),     #{ strategy  => one_for_one
                                          , intensity => Intensity
                                          , period    => Period
                                          }).
-define(CHILD(Name, Type),     #{ id       => Name
                               , start    => {Name, start_link, []}
                               , restart  => temporary
                               , shutdown => 5000
                               , type     => Type
                               , modules  => [Name]
                               }).
%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Start achlys top level supervisor.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() ->
{ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
-spec init(any()) ->
  {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
  {ok, {?SUPFLAGS(?THREE, ?TEN), ?CHILD(achlys_pmod_nav, worker)}}.

%%====================================================================
%% Internal functions
%%====================================================================
