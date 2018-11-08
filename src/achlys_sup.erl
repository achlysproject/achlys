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
    {ok , pid()} | ignore | {error , {already_started , pid()} | {shutdown , term()} | term()}.
start_link() ->
    % {ok , NavConfig} = achlys_config:get(temperature) ,
    % supervisor:start_link({local , ?SERVER} , ?MODULE , [NavConfig]).
    supervisor:start_link({local , ?SERVER} , ?MODULE , []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
%% @private
-spec init(term()) ->
    {ok , {supervisor:sup_flags() , [supervisor:child_spec()]}} | ignore.
init([]) ->
    {ok , {?SUPFLAGS(?THREE , ?TEN) , [
          ?NAV_WORKER
        , ?CLEANER_WORKER]}}.
    % IsMap = is_map(Args) ,
    % case IsMap of
    %     true ->
    %         {ok , {?SUPFLAGS(?THREE , ?TEN) , [
    %               ?CHILD(achlys_pmod_nav_worker , worker , [Args])
    %             , ?CHILD(achlys_cleaner , worker , [])]}};
    %             %   ?CHILD({achlys_pmod_nav_worker , worker , [Args]}, transient, ?FIVE, worker)
    %             % , ?CHILD({achlys_cleaner , worker , []), permanent, ?THREE, worker}
    %             % ]}
    %         % };
    %     _ ->
    %         ignore
    % end.

%%====================================================================
%% Internal functions
%%====================================================================
