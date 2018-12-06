%%%-------------------------------------------------------------------
%%% @author Igor Kopestenski <igor.kopestenski@uclouvain.be>
%%%     [https://github.com/Laymer/achlys]
%%% @doc
%%% Utility module mainly for shorthands and conversions.
%%% @end
%%% Created : 06. Nov 2018 21:32
%%%-------------------------------------------------------------------
-module(achlys_util).
-author("Igor Kopestenski <igor.kopestenski@uclouvain.be>").

-include("achlys.hrl").

-compile({nowarn_export_all}).
-compile(export_all).

%%====================================================================
%% Utility functions
%%====================================================================

query(Id) ->
    {ok , S} = lasp:query(Id) ,
    sets:to_list(S).

query(Name , Type) ->
    BitString = atom_to_binary(Name , utf8) ,
    {ok , S} = lasp:query({BitString , Type}) ,
    sets:to_list(S).

declare_crdt(Name , Type) ->
    Bitstring = atom_to_binary(Name , utf8) ,
    {ok , {Id , _ , _ , _}} = lasp:declare({Bitstring , Type} , Type) ,
    Id.

do_gc() ->
    _ = [erlang:garbage_collect(X , [{type , 'major'}]) || X <- erlang:processes()] ,
    ok.

gc_info() ->
    statistics(garbage_collection).

get_temp() ->
    pmod_nav:read(acc , [out_temp]).

%% @todo reduce interleavings between modules
%% and perform function calls without shortcuts in @module
table(Name) ->
    ets:new(Name , [ordered_set ,
                    named_table ,
                    public ,
                    {heir , whereis(achlys_sup) , []}]).

insert_timed_key(TableName , Value) ->
    ets:insert(TableName , {erlang:monotonic_time() , Value}).

store_temp() ->
    ets:insert(temp , {erlang:monotonic_time() , pmod_nav:read(acc , [out_temp])}).

grisp() ->
    application:ensure_all_started(grisp).

time() ->
    logger:log(notice , "Time : ~p ~n" , [erlang:monotonic_time()]).

remotes_to_atoms([H | T]) ->
    C = unicode:characters_to_list(["achlys@" , H]) ,
    R = list_to_atom(C) ,
    [R | remotes_to_atoms(T)];
remotes_to_atoms([]) ->
    [].

fetch_resolv_conf() ->

    {ok , F} = case os:type() of
                   {unix , rtems} ->
                       {ok , "nofile"};
                   _ ->
                       {ok , Cwd} = file:get_cwd() ,
                       Wc = filename:join(Cwd , "**/files/") ,
                       filelib:find_file("erl_inetrc" , Wc)
               end ,
    ok = inet_db:add_rc(F) ,
    inet_db:get_rc().

get_packet(Size) ->
    <<1:Size>>.

bitstring_name() ->
    atom_to_binary(node() , utf8).

get_inet_least_significant() ->
  {ok, [{{_,_,_,In},_,_}|T]} = inet:getif(),
  integer_to_binary(In).

%% https://stackoverflow.com/a/12795014/6687529
random_bytes(Bytes) ->
  re:replace(base64:encode(crypto:strong_rand_bytes(Bytes)),"\\W","",[global,{return,binary}]).

declare_awset(Name) ->
    String = atom_to_list(Name) ,
    AWName = list_to_bitstring(String) ,
    AWSetType = state_awset ,
    {ok , {AWSet , _ , _ , _}} = lasp:declare({AWName , AWSetType} , AWSetType) ,
    application:set_env(achlys , awset , AWSet) ,
    AWSet.

do_ping(1) ->
    net_adm:ping(achlys@my_grisp_board_1);
do_ping(2) ->
    net_adm:ping(achlys@my_grisp_board_2);
do_ping(3) ->
    net_adm:ping(achlys@my_grisp_board_3);
do_ping(4) ->
    net_adm:ping(achlys@my_grisp_board_4);
do_ping(5) ->
    net_adm:ping(achlys@my_grisp_board_5);
do_ping(6) ->
    net_adm:ping(achlys@my_grisp_board_6).

do_disconnect(1) ->
    net_kernel:disconnect(achlys@my_grisp_board_1);
do_disconnect(2) ->
    net_kernel:disconnect(achlys@my_grisp_board_2);
do_disconnect(3) ->
    net_kernel:disconnect(achlys@my_grisp_board_3);
do_disconnect(4) ->
    net_kernel:disconnect(achlys@my_grisp_board_4);
do_disconnect(5) ->
    net_kernel:disconnect(achlys@my_grisp_board_5);
do_disconnect(6) ->
    net_kernel:disconnect(achlys@my_grisp_board_6).

sample_task() ->
    F = sample_function(),
    #{
        name => sample_task,
        targets => all,
        function => F
    }.

sample_function() ->
    fun F() -> io:format("Sample function ! ~n") end.
%%====================================================================
%% Snippets
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
