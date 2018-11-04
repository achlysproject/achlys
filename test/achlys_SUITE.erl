%%%-------------------------------------------------------------------
%%% File    : achlys_SUITE.erl
%%% Author  : "Igor Kopestenski <igor.kopestenski@uclouvain.be"
%%% Description : achlys test suite
%%%
%%% Created : @date
%%%-------------------------------------------------------------------
-module(achlys_SUITE).
-author("Igor Kopestenski <igor.kopestenski@uclouvain.be").

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Test server callbacks
-export([suite/0
  , all/0
  , init_per_suite/1
  , end_per_suite/1
  , init_per_testcase/2
  , end_per_testcase/2]).

-compile(export_all).

%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_suite(Config) ->
  application:set_env(grisp, drivers, [
      {spi, grisp_spi_drv_emu}
  ]),
  application:set_env(grisp, devices, [
      {spi1, pmod_nav}
  ]),
  {ok, _Deps} = application:ensure_all_started(grisp),
  Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    application:stop(grisp),
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() ->
    [
      nav_worker_test_temperature,
      nav_worker_test_pressure,
      nav_worker_test_mag,
      nav_worker_test
    ].

%%--------------------------------------------------------------------
%% Function: TestCase() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------
nav_worker_test() ->
    ok.

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%%--------------------------------------------------------------------
nav_worker_test_temperature() ->
    T = pmod_nav:read(acc, [out_temp]),
    ?assertEqual([25.0], T),
    ok.


nav_worker_test_pressure() ->
  P = pmod_nav:read(alt, [out_press]),
  ?assertEqual([0.0], P),
  ok.

%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%%--------------------------------------------------------------------
nav_worker_test_mag() ->
  Mag = [_X,_Y,_Z] = pmod_nav:read(mag, [out_x_m, out_y_m, out_z_m]),
  ?assertEqual([0.0, 0.0, 0.0], Mag),
  ok.
