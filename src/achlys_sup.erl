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
    {ok , {?SUPFLAGS(?THREE , ?TEN) , [
          % ?NAV_WORKER
          ?SENSOR_COMMANDER
        , ?CLEANER_WORKER
          % , ?CLEANER_WORKER]}}.
        , ?TASK_SERVER
        , ?SQUADRON_LEADER]}}.

%%====================================================================
%% Internal functions
%%====================================================================
