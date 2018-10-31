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

-define(SERVER, ?MODULE).

%% Thanks to https://github.com/erszcz
%% Helper macro for declaring children of supervisor
% -define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(SUPFLAGS(Intensity, Period),   #{ strategy  => one_for_one
                                        , intensity => Intensity
                                        , period    => Period
                                        }).

-define(CHILD(Name, Type, Args),         #{ id     => Name
                                        , start    => {Name, start_link, [Args]}
                                        , restart  => temporary
                                        , shutdown => 5000
                                        , type     => Type
                                        , modules  => [Name]
                                        }).
%%====================================================================
%% API functions
%%====================================================================

% {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
%% @doc Start achlys top level supervisor.
-spec start_link() ->
  {ok, pid()} | ignore | {error, {already_started, pid()} | {shutdown, term()} | term()}.
start_link() ->
  {ok, NavConfig} = achlys_config:get(temperature),
  supervisor:start_link({local, ?SERVER}, ?MODULE, [NavConfig]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
%% @private
-spec init(term()) ->
  {ok,{supervisor:sup_flags(),[supervisor:child_spec()]}} | ignore.
init([Args]) ->
  {ok, {?SUPFLAGS(?THREE, ?TEN), [?CHILD(achlys_pmod_nav_worker, worker, Args)]}}.

%%====================================================================
%% Internal functions
%%====================================================================
