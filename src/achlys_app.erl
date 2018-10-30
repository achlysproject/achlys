%%%-------------------------------------------------------------------
%%% @doc The Achlys application module
%%% @end
%%%-------------------------------------------------------------------

-module(achlys_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, Supervisor} = achlys_sup:start_link(),

    %% Once the LEDs have turned red,
    %% the supervisor has been initialized.
    LEDs = [1, 2],
    [grisp_led:color(L, red) || L <- LEDs],

    {ok, Supervisor}.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
