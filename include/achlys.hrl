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

-define(NAV_WORKER , #{id     => achlys_pmod_nav_worker
    , start    => {achlys_pmod_nav_worker , start_link , []}
    , restart  => transient
    , shutdown => 5000
    , type     => worker
    , modules  => [achlys_pmod_nav_worker]
}).

-define(ALS_WORKER , #{id     => achlys_pmod_als_worker
    , start    => {achlys_pmod_als_worker , start_link , []}
    , restart  => transient
    , shutdown => 5000
    , type     => worker
    , modules  => [achlys_pmod_als_worker]
}).

-define(CLEANER_WORKER , #{id     => achlys_cleaner
    , start    => {achlys_cleaner , start_link , []}
    , restart  => permanent
    , shutdown => 5000
    , type     => worker
    , modules  => [achlys_cleaner]
}).

-define(SENSOR_COMMANDER , #{id     => achlys_sensor_commander
    , start    => {achlys_sensor_commander , start_link , []}
    , restart  => permanent
    , shutdown => 5000
    , type     => worker
    , modules  => [achlys_sensor_commander]
}).

-define(TASK_SERVER , #{id     => achlys_task_server
    , start    => {achlys_task_server , start_link , []}
    , restart  => permanent
    , shutdown => 5000
    , type     => worker
    , modules  => [achlys_task_server]
}).

-define(TASK_WORKER , #{id     => achlys_task_worker
    , start    => {achlys_task_worker , start_link , []}
    , restart  => permanent
    , shutdown => 5000
    , type     => worker
    , modules  => [achlys_task_worker]
}).

-define(SQUADRON_LEADER , #{id     => achlys_squadron_leader
    , start    => {achlys_squadron_leader , start_link , []}
    , restart  => permanent
    , shutdown => 5000
    , type     => worker
    , modules  => [achlys_squadron_leader]
}).

-define(SQUADRON_WORKER , #{id     => achlys_squadron_worker
    , start    => {achlys_squadron_worker , start_link , []}
    , restart  => temporary
    , shutdown => 5000
    , type     => worker
    , modules  => [achlys_squadron_worker]
}).

-define(PMOD_WORKER_SUPERVISOR , #{id     => achlys_pmod_worker_sup
    , start    => {achlys_pmod_worker_sup , start_link , []}
    , restart  => permanent
    , shutdown => 5000
    , type     => supervisor
    , modules  => [achlys_pmod_worker_sup]
}).

-define(WORKERS, #{clustering => ?SQUADRON_LEADER
    , cleaning => ?CLEANER_WORKER
    , sensing => ?SENSOR_COMMANDER
    , squadron_worker => ?SQUADRON_WORKER
}).

-define(STREAMERS, #{pmod_nav => ?NAV_WORKER
    , pmod_als => ?ALS_WORKER
}).

-define(TEMP_LIST , [{<<"achlys@LaymerMac_temperature">>, state_awset}
                    , {<<"achlys@my_grisp_board_1_temperature">>, state_awset}
                    , {<<"achlys@my_grisp_board_2_temperature">>, state_awset}
                    , {<<"achlys@my_grisp_board_3_temperature">>, state_awset}
                    , {<<"achlys@my_grisp_board_4_temperature">>, state_awset}
                    , {<<"achlys@my_grisp_board_5_temperature">>, state_awset}
                    , {<<"achlys@my_grisp_board_6_temperature">>, state_awset}]).

-define(PRESS_LIST , [{<<"achlys@LaymerMac_pressure">>, state_awset}
                    , {<<"achlys@my_grisp_board_1_pressure">>, state_awset}
                    , {<<"achlys@my_grisp_board_2_pressure">>, state_awset}
                    , {<<"achlys@my_grisp_board_3_pressure">>, state_awset}
                    , {<<"achlys@my_grisp_board_4_pressure">>, state_awset}
                    , {<<"achlys@my_grisp_board_5_pressure">>, state_awset}
                    , {<<"achlys@my_grisp_board_6_pressure">>, state_awset}]).

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
