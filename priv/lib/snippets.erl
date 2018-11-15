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

ets:match(node()).
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
