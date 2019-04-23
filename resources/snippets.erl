(achlys@Igors-MacBook-Pro-2)15> nc -u -b 255.255.255.255 9998
(achlys@Igors-MacBook-Pro-2)15> gen_udp:send(S, {255,255,255,255}, 9998, <<"bcast test msg">>).
* 1: syntax error before: 255.255

(achlys@Igors-MacBook-Pro-2)15> {ok, S} = gen_udp:open( 0, [binary, {broadcast, true}]).
** exception error: no match of right hand side value {ok,#Port<0.14>}
(achlys@Igors-MacBook-Pro-2)16> gen_udp:send(S, {255,255,255,255}, 9998, <<"bcast test msg">>).
{error,closed}
(achlys@Igors-MacBook-Pro-2)17> {ok, S2} = gen_udp:open( 0, [binary, {broadcast, true}]).
{ok,#Port<0.15>}
(achlys@Igors-MacBook-Pro-2)18> gen_udp:send(S, {255,255,255,255}, 9998, <<"bcast test msg">>).
{error,closed}
(achlys@Igors-MacBook-Pro-2)19> gen_udp:send(S2, {255,255,255,255}, 9998, <<"bcast test msg">>).
ok
(achlys@Igors-MacBook-Pro-2)20> gen_udp:send(S2, {255,255,255,255}, 9998, <<"bcast test msg">>).
ok

(achlys@Igors-MacBook-Pro-2)48> gen_udp:send(S2, {255,255,255,255}, 9998, <<"bcast test msg">>).
ok
(achlys@Igors-MacBook-Pro-2)49> [spawn(fun() -> gen_udp:send(S2, {255,255,255,255}, 9998, <<"bcast test msg">>) end) || X <- lists:se
q(1,10000)].
[<0.150.0>,<0.151.0>,<0.152.0>,<0.153.0>,<0.154.0>,
 <0.155.0>,<0.156.0>,<0.157.0>,<0.158.0>,<0.159.0>,<0.160.0>,
 <0.161.0>,<0.162.0>,<0.163.0>,<0.164.0>,<0.165.0>,<0.166.0>,
 <0.167.0>,<0.168.0>,<0.169.0>,<0.170.0>,<0.171.0>,<0.172.0>,
 <0.173.0>,<0.174.0>,<0.175.0>,<0.176.0>,<0.177.0>,<0.178.0>|...]
(achlys@Igors-MacBook-Pro-2)50> [spawn(fun() -> gen_udp:send(S2, {255,255,255,255}, 9998, <<"bcast test msg">>) end) || X <- lists:se
q(1,10000)].
[<0.10151.0>,<0.10152.0>,<0.10153.0>,<0.10154.0>,
 <0.10155.0>,<0.10156.0>,<0.10157.0>,<0.10158.0>,<0.10159.0>,
 <0.10160.0>,<0.10161.0>,<0.10162.0>,<0.10163.0>,<0.10164.0>,
 <0.10165.0>,<0.10166.0>,<0.10167.0>,<0.10168.0>,<0.10169.0>,
 <0.10170.0>,<0.10171.0>,<0.10172.0>,<0.10173.0>,<0.10174.0>,
 <0.10175.0>,<0.10176.0>,<0.10177.0>,<0.10178.0>,<0.10179.0>|...]
(achlys@Igors-MacBook-Pro-2)51> [spawn(fun() -> gen_udp:send(S2, {255,255,255,255}, 9998, <<"bcast test msg">>) end) || X <- lists:se
q(1,1000000)].


grisp:add_device(uart,pmod_maxsonar).
pmod_maxsonar:get().
pmod_als:percentage().
pmod_nav:read(alt, [temp_out]).
pmod_nav:read(mag, [out_x_m, out_y_m, out_z_m]).
spawn(fun() -> io:format("HYGRO : ~p ", [pmod_hygro:humid()]) end ).

%%====================================================================
%% igor:create_stubs attempts
%%====================================================================

igor:create_stubs({achlys,[
    {join, {lasp_peer_service, join} },
    {join, {lists, foldl} },
]},[{backups,true}]).


%%====================================================================
%% Emulator erts_alloc flags
%%====================================================================

+MHas aobf +MMmcs 10 +MMamcbf 1024 +MHlmbcs 128

%%====================================================================
%% Code snippets useful for erlang shell calls.
%%====================================================================

Self = partisan_hyparview_peer_service_manager:myself().
Self = partisan_hyparview_peer_service_manager:myself(),R2 = rpc:call(achlys@my_grisp_board_2,partisan_hyparview_peer_service_manager,myself,[]).
Self = partisan_hyparview_peer_service_manager:myself(),R6 = rpc:call(achlys@my_grisp_board_6,partisan_hyparview_peer_service_manager,myself,[]).
Self = partisan_hyparview_peer_service_manager:myself(),R5 = rpc:call(achlys@my_grisp_board_5,partisan_hyparview_peer_service_manager,myself,[]).
Self = partisan_hyparview_peer_service_manager:myself(),R4 = rpc:call(achlys@my_grisp_board_4,partisan_hyparview_peer_service_manager,myself,[]).
R5 = rpc:call(achlys@my_grisp_board_5,partisan_hyparview_peer_service_manager,myself,[]).
R3 = rpc:call(achlys@my_grisp_board_3,partisan_hyparview_peer_service_manager,myself,[]).
R1 = rpc:call(achlys@my_grisp_board_1,partisan_hyparview_peer_service_manager,myself,[]).
R2 = rpc:call(achlys@my_grisp_board_2,partisan_hyparview_peer_service_manager,myself,[]).
lasp_peer_service:join(R5).
lasp_peer_service:join(R3).
lasp_peer_service:join(R1).
lasp_peer_service:join(R2),rpc:call(achlys@my_grisp_board_2,lasp_peer_service,join,[Self]).
lasp_peer_service:join(R6),rpc:call(achlys@my_grisp_board_6,lasp_peer_service,join,[Self]).
lasp_peer_service:join(R4),rpc:call(achlys@my_grisp_board_4,lasp_peer_service,join,[Self]).
lasp_peer_service:join(R5),rpc:call(achlys@my_grisp_board_5,lasp_peer_service,join,[Self]).
rpc:call(achlys@my_grisp_board_5,lasp_peer_service,join,[Self]).
rpc:call(achlys@my_grisp_board_1,lasp_peer_service,join,[Self]).
achlys:members().


T = #{name => t1,targets => <<0>>,execution_type => <<0>>,function => fun() -> io:format("hi :) ~n") end}.
T3 = #{name => t3,targets => <<0>>,execution_type => <<1>>,function => fun() -> pmod_nav:read(alt, [press_out]) end}.
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
inet_db:add_rc("/Users/Laymer/EdgeComputing/New/achlys/grisp/grisp_base/files/erl_inetrc").
nodes().
node().
ets:delete(node(),{<<"achlys@LaymerMac_pressure">>,state_awset}).
achlys:clusterize().
lasp_peer_service:join(achlys@my_grisp_board_2),lasp_peer_service:join(achlys@my_grisp_board_3).
lasp_peer_service:join(achlys@my_grisp_board_1),lasp_peer_service:join(achlys@my_grisp_board_3).
lasp_peer_service:join(achlys@my_grisp_board_2),lasp_peer_service:join(achlys@my_grisp_board_1).
achlys:venom().

inet:i().

lasp_peer_service:join(achlys@my_grisp_board_2).
lasp_peer_service:join(achlys@LaymerMac).
lasp_peer_service:join(achlys@my_grisp_board_3),lasp_peer_service:join(achlys@my_grisp_board_1).
lasp_peer_service:join(achlys@my_grisp_board_2),lasp_peer_service:join(achlys@my_grisp_board_4).
lasp_peer_service:join(achlys@my_grisp_board_1),lasp_peer_service:join(achlys@my_grisp_board_4).
lasp_peer_service:join(achlys@my_grisp_board_5).
lasp_peer_service:join(achlys@my_grisp_board_6).
achlys:members().

achlys:bane(temperature).
achlys_util:get_temperatures().

lasp_peer_service:members().
lasp:query({<<"temperature">>, state_awset}).
atom_to_binary(temperature, utf8).

{ok, {AWPSSet, _, _, _}} = lasp:declare({<<"set">>,state_awset_ps}, state_awset_ps).
lasp:update(AWPSSet, {add, one}, self()).
lasp:query({<<"set">>,state_awset_ps}).
rp(lists:usort(achlys_util:query({<<"temperature">>,state_awset}))).
{ok,S2}=lasp:query({<<"achlys@my_grisp_board_2_temperature">>,state_awset}),sets:to_list(S2).
{ok,S3}=lasp:query({<<"achlys@my_grisp_board_1_temperature">>,state_awset}),sets:to_list(S3).
{ok,S4}=lasp:query({<<"achlys@my_grisp_board_3_temperature">>,state_awset}),sets:to_list(S4).
lasp:query({<<"achlys@my_grisp_board_2_temperature">>,state_awset}).
lasp:query({<<"achlys@my_grisp_board_4_temperature">>,state_awset}).
lasp:query({<<"achlys@my_grisp_board_5_temperature">>,state_awset}).
lasp:query({<<"achlys@my_grisp_board_3_temperature">>,state_awset}).
lasp:query({<<"achlys@my_grisp_board_1_temperature">>,state_awset}).
ets:match(node()).

rpc:call(achlys@my_grisp_board_6, achlys, bane_all_preys, [temperature]).
rpc:call(achlys@my_grisp_board_6, rpc, call, [achlys@my_grisp_board_3, achlys, bane_all_preys, [temperature]]).

%%====================================================================
%% Snippets from achlys_sup module
%%====================================================================

% {ok , {?SUPFLAGS(?THREE , ?TEN) , [
%     %   ?SENSOR_COMMANDER
%     % , ?CLEANER_WORKER
%     , ?TASK_SERVER]}}.
% , ?SQUADRON_LEADER]}}.


% case maps:next(WorkersList) of
%     {K, true, NextIter} ->
%         extract_child_specs(K),
%         body;
%     % {K, true, none} ->
%     %     body;
%     none ->
%         []
% end.

%% @private
% extract_child_specs(K) ->


%% @private
% -spec squadron_leader_specs() -> [supervisor:child_spec()] | [].
% squadron_leader_specs() ->
%     case achlys_config:get(clustering) of
%         true ->
%             [?SQUADRON_LEADER];
%         _ ->
%             []
%     end.

% IsMap = is_map(Args) ,
% case IsMap of
%     true ->
%         {ok , {?SUPFLAGS(?THREE , ?TEN) , [
%               ?CHILD(achlys_pmod_nav_worker , worker , [Args])
%             , ?CHILD(achlys_cleaner , worker , [])]}};
%             %   ?CHILD({achlys_pmod_nav_worker , worker , [Args]}, transient, ?FIVE, worker)
%             % , ?CHILD({achlys_cleaner , worker , []), permanent, ?THREE, worker}
%             % ]}
%         % };
%     _ ->
%         ignore
% end.

%%====================================================================
%% Snippets from sys.config
%%====================================================================

% see https://github.com/lasp-lang/lasp/commit/a9422207e554e47495c7c488450a238274844eb9
% NOTE : propagate_on_update not available in delta based mode?
% {propagate_on_update, true}

%%====================================================================
%% Snippets from achlys module
%%====================================================================

%% WIP

%% @doc Returns a list of known remote hostnames
%% that could be potential neighbors, limited to maximum active view size.
% get_bounded_preys() ->
% TODO : attempt to reach up to MaxActiveSize possible neighbors from list
%     L = lists:usort(binary_remotes_to_atoms(seek_neighbors())),
%     Len = length(L),
%     {ok, MaxActiveSize} = application:get_env(partisan, max_active_size),
%     case Len > MaxActiveSize of
%         true ->
%             {H, _T} = lists:split(MaxActiveSize, L),
%             H;
%         false ->
%             L
%     end.


% %% @private
% join(Host) ->
%     Manager = rpc:call(Host , partisan_peer_service , manager , []) ,
%     case Manager of
%         partisan_hyparview_peer_service_manager ->
%             Node = rpc:call(Host , Manager , myself , []) ,
%             ok = partisan_peer_service:join(Node) ,
%             logger:log(info , "Joined ~p~n" , [Host]) ,
%             {ok , Node};
%         {badrpc , Reason} ->
%             logger:log(error , "Unable to RPC remote : ~p~n" , [Reason]) ,
%             {error , Reason};
%         {error , Reason} ->
%             logger:log(error , "Unable to retrieve remote : ~p~n" , [Manager]) ,
%             {error , Reason}
%     end.

%%====================================================================
%% Snippets from achlys_util module
%%====================================================================


% foo(N, Bin) ->
%    <<X:N,T/binary>> = Bin,
%    {X,T}.
% <<Packet:Size/bitstring>> = <<1:Size>>.

%% NOTE : recon_alloc utility snippets :
%%recon() ->
%%    % L = recon_alloc:sbcs_to_mbcs(current),
%%    [ io:format("sbcs_to_mbcs === ~p ~n", [X]) || X <- recon_alloc:sbcs_to_mbcs(current) ],
%%    recon(avg).
%%recon(avg) ->
%%    [ io:format("average_block_sizes === ~p ~n", [X]) || X <- recon_alloc:average_block_sizes(current) ],
%%    recon(usage);
%%recon(usage) ->
%%    io:format("memory === ~p ~n", [recon_alloc:memory(usage,current)]),
%%    recon(cache);
%%recon(cache) ->
%%    recon_alloc:cache_hit_rates().
%%
%%maxrecon() ->
%%    % L = recon_alloc:sbcs_to_mbcs(current),
%%    [ io:format("sbcs_to_mbcs === ~p ~n", [X]) || X <- recon_alloc:sbcs_to_mbcs(current) ],
%%    maxrecon(avg).
%%maxrecon(avg) ->
%%    [ io:format("average_block_sizes === ~p ~n", [X]) || X <- recon_alloc:average_block_sizes(current) ],
%%    maxrecon(usage);
%%maxrecon(usage) ->
%%    io:format("memory === ~p ~n", [recon_alloc:memory(usage,current)]),
%%    maxrecon(cache);
%%maxrecon(cache) ->
%%    recon_alloc:cache_hit_rates().

%% Stress intensity
%% achlys_util:stress_throughput().
% stress_throughput() ->
%     stress_throughput(achlys_config:get(packet_config)).
% stress_throughput(#{stress_delay    := Interval,
%                 operations_count     := Count,
%                 packet_size          := Size}) ->
%     ((Count * Size) * (Interval / (?MILLION))). %% returns float
%     % (Count * Size) div ?MILLION.

% T.
% -spec maybe_declare_crdt(atom(), atom()) -> atom().
% maybe_declare_crdt(Name, Type) ->
%     case achlys_util:query(Name, Type) of
%         [] ->
%             Bitstring = atom_to_binary(Name , utf8) ,
%             {ok , {Id , _ , _ , _}} = lasp:declare({Bitstring , Type} , Type) ,
%             Id;
%         _ ->
%             Name
%     end.
% declare_crdt(Name , Type) ->


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
%%

% lasp:declare({<<"set">>,state_awset_ps},state_awset_ps).
% lasp:declare({<<"set">>,state_awset},state_awset).
% lasp_peer_service:join(achlys@LaymerMac).
% lasp:join(#{channels => [undefined],listen_addrs => [#{ip => {169,254,16,6},port => 17768}],name => achlys@my_grisp_board_6,parallelism => 1}).
% net_adm:ping(achlys@LaymerMac).
% lasp:update({<<"set">>,state_awset_ps},{add, test},self()).
% lasp:update({<<"set">>,state_awset},{add, test},self()).
% lasp:query({<<"set">>,state_awset_ps}).
% lasp:query({<<"set">>,state_awset}).
% lasp:query({<<"achlys@my_grisp_board_1_temperature">>,state_awset}).
% dets:sync(achlys@my_grisp_board_1).
% dets:sync(node()).
