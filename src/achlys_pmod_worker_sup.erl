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
-export([init/1]).
% -export([start_worker/1]).
% -export([stop_worker/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% -spec start_worker(node()) -> {ok, pid()}.
% start_worker(Node) ->
%     supervisor:start_child(?MODULE, [Node]).
%
% -spec stop_worker(pid()) -> ok | {error, not_found}.
% stop_worker(Pid) ->
%     supervisor:terminate_child(?MODULE, Pid).

init([]) ->
	{ok, {{one_for_one, ?THREE, ?TEN}, [?NAV_WORKER]}}.
