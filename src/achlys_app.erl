%%%-------------------------------------------------------------------
%% @doc The Achlys application module
%% @end
%%%-------------------------------------------------------------------
-module(achlys_app).
-author("Igor Kopestenski <igor.kopestenski@uclouvain.be>").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, Supervisor} = achlys_sup:start_link(),

    % For test purposes, grisp_app allows calls to emulated pmod_nav
    % in an Erlang shell when the "drivers" configuration parameter specifies
    % only elements with the "_emu" suffix for each slot.
    {ok, _} = application:ensure_all_started(grisp),
    %% Once the LEDs have turned red,
    %% the supervisor has been initialized.
    LEDs = [1, 2],
    [grisp_led:color(L, red) || L <- LEDs],

    {ok, Supervisor}.

%%--------------------------------------------------------------------
stop(_State) ->
    achlys:stop(),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
