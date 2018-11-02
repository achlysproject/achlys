% @doc Top level module launching the achlys app.
% @end
-module(achlys).

-export([start/0]).
-export([stop/0]).

%% ===================================================================
%% Entry point functions
%% ===================================================================

%% @doc Start the application.
-spec start() -> ok.
start() ->
  {ok, _} = application:ensure_all_started(achlys),
  ok.

%% @doc Stop the Application
-spec stop() -> ok.
stop() ->
  ok = application:stop(achlys),
  ok.
