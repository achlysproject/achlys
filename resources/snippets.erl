%%====================================================================
%% Code snippets useful for erlang shell calls.
%%====================================================================

pmod_nav:read(acc , [out_temp]).
pmod_nav:read(alt , [press_out]).
pmod_als:read().
pmod_als:percentage().

inet:getif().

achlys:aggregate_sensor_data().
achlys:get_aggregate(temperature).
achlys:get_aggregate(pressure).
rp(achlys:get_aggregate(temperature)).
rp(achlys:get_aggregate(pressure)).

ets:match(temperature,'$1').
ets:match(pressure,'$1').

erlang:memory().

nodes().
node().

achlys:clusterize().

inet:i().

lasp_peer_service:members().
lasp:query({<<"temperature">>, state_awset}).
atom_to_binary(temperature, utf8).

{ok, {AWPSSet, _, _, _}} = lasp:declare({<<"set">>,state_awset_ps}, state_awset_ps).
lasp:update(AWPSSet, {add, one}, self()).
lasp:query({<<"set">>,state_awset_ps}).

ets:match(node()).

% achlys.hrl :
% -define(CHILD(Restart , Shutdown , Type , {M,F,A}) ,
%     #{id     => M
%     , start    => {M , F , A}
%     , restart  => Restart
%     , shutdown => Shutdown
%     , type     => Type
%     , modules  => [M]
% }).

% -define(MFA(Name, Args),        {Name, start_link, Args}).

% L = [achlys@my_grisp_board_2,achlys@my_grisp_board_3,achlys@my_grisp_board_1,achlys@my_grisp_board_6,achlys@my_grisp_board_5,achlys@my_grisp_board_4].
% achlys:contagion().
% achlys:clusterize().
% achlys:members().
% achlys:venom(),achlys:venom(achlys_pmod_als_worker).

% L = [achlys@LaymerMac,achlys2@LaymerMac,achlys3@LaymerMac].
% L = [achlys@my_grisp_board_1,achlys@my_grisp_board_3], M = partisan_hyparview_peer_service_manager [rpc:call(X,lasp_peer_service,join,[partisan_hyparview_peer_service_manager:myself()]) || X <- L ].
%
% rpc:call(achlys@LaymerMac,lasp_peer_service,join,[partisan_hyparview_peer_service_manager:myself()]).
% lasp_peer_service:members().
% achlys:bane(temperature).
%
% Self = partisan_hyparview_peer_service_manager:myself(),L = [achlys@my_grisp_board_2,achlys@my_grisp_board_3],M = partisan_hyparview_peer_service_manager.
% [lasp_peer_service:join(rpc:call(X,M,myself,[])) || X <- L],[rpc:call(X,lasp_peer_service,join,[Self]) || X <- L].
%
% lasp_peer_service:join(rpc:call(achlys@my_grisp_board_3,M,myself,[])),rpc:call(achlys@my_grisp_board_3,lasp_peer_service,join,[Self]).
% lasp:update({<<"temperature">>,state_awset},{rmv, {<<"2">>,temperature,{119,20.175945378151262}}},self()).
% achlys:venom(),achlys:venom(achlys_pmod_als_worker).
% achlys:bane(temperature).
% achlys:bane(light).
% achlys_util:do_gc().
%

%% from riak_core :
%% @doc Wraps an rpc:call/4 in a try/catch to handle the case where the
%%      'rex' process is not running on the remote node. This is safe in
%%      the sense that it won't crash the calling process if the rex
%%      process is down.
% -spec safe_rpc(Node :: node(), Module :: atom(), Function :: atom(),
%         Args :: [any()]) -> {'badrpc', any()} | any().
% safe_rpc(Node, Module, Function, Args) ->
%     try rpc:call(Node, Module, Function, Args) of
%         Result ->
%             Result
%     catch
%         exit:{noproc, _NoProcDetails} ->
%             {badrpc, rpc_process_down}
%     end.


% case check_streams(Streams) of
%     {ok, [Ks]} ->
%         maybe_run_workers([Ks]);
%     _ ->
%         %% retry in case configuration
%         %% has been changed at runtime
% end,
% erlang:send_after(?THREEMIN , ?SERVER , {setup_stream_workers}}),
% maybe_run_workers(Streams),


% lasp:declare({<<"set">>,state_awset_ps},state_awset_ps).
% lasp_peer_service:join(achlys@LaymerMac).
% lasp:join(#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,6},port => 17768}],name => achlys@my_grisp_board_6,parallelism => 1}).
% net_adm:ping(achlys@LaymerMac).
% lasp:update({<<"set">>,state_awset_ps},{add, test},self()).
% lasp:query({<<"set">>,state_awset_ps}).
