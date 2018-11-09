%%====================================================================
%% @doc achlys constants definitions
%% @end
%%====================================================================

-include_lib("kernel/include/file.hrl").

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
%% Common Macross
%%====================================================================

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
% -define(CHILD(Restart , Shutdown , Type , {M,F,A}) ,
%     #{id     => M
%     , start    => {M , F , A}
%     , restart  => Restart
%     , shutdown => Shutdown
%     , type     => Type
%     , modules  => [M]
% }).

% -define(MFA(Name, Args),        {Name, start_link, Args}).
