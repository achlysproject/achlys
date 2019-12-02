%%====================================================================
%% @doc achlys constants definitions
%% @end
%%====================================================================

-include_lib("kernel/include/file.hrl").

-export_type([task_targets/0]).
-export_type([task_execution_type/0]).
-export_type([task/0]).

%%====================================================================
%% Time Intervals (ms)
%%====================================================================

-define(MS , 20).
-define(ONE , 1000).
-define(THREE , 3000).
-define(FIVE , 5000).
-define(TEN , 10000).
-define(HMIN , 30000).
-define(MIN , 60000).
-define(TWOMIN , 120000).
-define(THREEMIN , 180000).

-define(MILLION , 1000000).

%%====================================================================
%% Common Macros
%%====================================================================

-define(TASKS , {<<"tasks">>, state_gset}).

-define(TARGET_ALL_NODES, <<0>>).
-define(PERMANENT_TASK, <<0>>).
-define(SINGLE_EXECUTION_TASK, <<1>>).

%% Thanks to https://github.com/erszcz
%% Helper macro for declaring children of supervisor
% -define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(SUPFLAGS(Intensity , Period) , #{strategy  => one_for_one
    ,                                    intensity => Intensity
    ,                                    period    => Period
}).

-define(CHILD(I, Type) , #{id     => I
    , start    => {I , start_link , []}
    , restart  => permanent
    , shutdown => 5000
    , type     => Type
    , modules  => [I]
}).

-define(NAV_WORKER , ?CHILD(achlys_pmod_nav_worker, worker)).
-define(ALS_WORKER , ?CHILD(achlys_pmod_als_worker, worker)).
-define(SENSOR_COMMANDER , ?CHILD(achlys_sensor_commander, worker)).

-define(STREAMERS, #{achlys_pmod_nav_worker => ?NAV_WORKER
    , achlys_pmod_als_worker => ?ALS_WORKER
}).

%%====================================================================
%% Types
%%====================================================================

% -type task_targets() :: [node()] | ?TARGET_ALL_NODES.
-type task_targets() :: [node()] | bitstring().

% -type task_execution_type() :: ?PERMANENT_TASK | ?SINGLE_EXECUTION_TASK.
-type task_execution_type() :: bitstring().

-type task() :: #{name => atom(),
                    targets => task_targets(),
                    execution_type => task_execution_type(),
                    function => function()}.

%%====================================================================
%% Misc
%%====================================================================
-define(TEMP_LIST , [{<<"achlys@my_grisp_board_1_temperature">>, state_awset}
                , {<<"achlys@my_grisp_board_2_temperature">>, state_awset}
                , {<<"achlys@my_grisp_board_3_temperature">>, state_awset}
                , {<<"achlys@my_grisp_board_4_temperature">>, state_awset}
                , {<<"achlys@my_grisp_board_5_temperature">>, state_awset}
                , {<<"achlys@my_grisp_board_6_temperature">>, state_awset}]).

-define(PRESS_LIST , [{<<"achlys@my_grisp_board_1_pressure">>, state_awset}
                , {<<"achlys@my_grisp_board_2_pressure">>, state_awset}
                , {<<"achlys@my_grisp_board_3_pressure">>, state_awset}
                , {<<"achlys@my_grisp_board_4_pressure">>, state_awset}
                , {<<"achlys@my_grisp_board_5_pressure">>, state_awset}
                , {<<"achlys@my_grisp_board_6_pressure">>, state_awset}]).