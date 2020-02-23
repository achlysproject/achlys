%%%-------------------------------------------------------------------
%%% @author Igor Kopestenski <igor.kopestenski@uclouvain.be>
%%%     [https://github.com/achlysproject/achlys]
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

query(Name , Type) when is_atom(Name), is_atom(Type) ->
    {ok , S} = lasp:query({atom_to_binary(Name , utf8) , Type}) ,
    sets:to_list(S).

query(Id) ->
    {ok , S} = lasp:query(Id) ,
    sets:to_list(S).

declare_crdt(Name , Type) ->
    Bitstring = atom_to_binary(Name , utf8) ,
    {ok , {Id , _ , _ , _}} = lasp:declare({Bitstring , Type} , Type) ,
    Id.

lager_level() ->
    % default to notice
    lager:set_loglevel(lager_console_backend, notice).

lager_level(ConsoleLevel) ->
    lager:set_loglevel(lager_console_backend, ConsoleLevel).

logger_level(LoggerPrimaryLevel) ->
    % OTP Kernel logger module log level
    logger:set_primary_config(level,LoggerPrimaryLevel).

%%====================================================================
%% Harware/IO helpers
%%====================================================================

gpio_pins() ->
    [% slot 1
    gpio1_1 
    , gpio1_2 
    , gpio1_3 
    , gpio1_4
    % slot 2
    , gpio2_1 
    , gpio2_2 
    , gpio2_3 
    , gpio2_4].

gpio_clear() ->
    [ grisp_gpio:clear(X) || X <- gpio_pins() ].

gpio_set_all() ->
    [ grisp_gpio:set(X) || X <- gpio_pins() ].

pmod_led_set(Count) ->
    Leds = lists:usort(gpio_pins()),
    [ grisp_gpio:set(X) || X <- lists:sublist(Leds, Count) ].

%%====================================================================
%% Benchmark helpers
%%====================================================================

gset(Name) -> 
    ?MODULE:declare_crdt(Name, state_gset).

orset(Name) -> 
    ?MODULE:declare_crdt(Name, state_orset).

add_to_orset(Value) ->
    lasp:update({<<"orset">>,state_orset}, {add, Value}, self()).

rmv_from_orset(Value) ->
    lasp:update({<<"orset">>,state_orset}, {rmv, Value}, self()).

get_orset() ->
    {ok, Set} = lasp:query({<<"orset">>,state_orset}) , 
    sets:to_list(Set).

populate_orset() ->
    fun() ->
        {ok, Backpressure} = achlys_config:get(backpressure_interval),
        ?MODULE:add_to_orset(orset_test_atom),
        erlang:send_after(Backpressure , self() , {added}),
        receive
            {added} ->
                logger:log(notice, "added ~n"),
                ?MODULE:rmv_from_orset(),
                erlang:send_after(Backpressure , self() , {removed});
            {removed} ->
                logger:log(notice, "removed ~n"),
                ?MODULE:add_to_orset(),
                erlang:send_after(Backpressure , self() , {added})
        after 10000 ->
            logger:critical("benchmark update timeout ~n"),
            logger:log(notice, "benchmark update timeout ~n")
        end
    end.

recon_info() ->
    CacheHitRates = recon_alloc:cache_hit_rates(),
    Frag = recon_alloc:fragmentation(current),
    Alloc = recon_alloc:memory(allocated),
    Used = recon_alloc:memory(used),
    Unused = recon_alloc:memory(unused),
    SingleToMultiBlockRatio = recon_alloc:sbcs_to_mbcs(current),
    [CacheHitRates
        ,Frag
        ,Alloc
        ,Used
        ,Unused
        ,SingleToMultiBlockRatio].

recon_info([CacheHitRates
        ,Frag
        ,Alloc
        ,Used
        ,Unused
        ,SingleToMultiBlockRatio]) ->
    logger:log(notice, "recon_alloc statistics : ~n 
        cache_hit_rates = ~p ~n
        Frag =  ~p ~n
        Alloc =  ~p ~n
        Used =  ~p ~n
        Unused =  ~p ~n
        SingleToMultiBlockRatio = ~p ~n"
        , [CacheHitRates
            ,Frag
            ,Alloc
            ,Used
            ,Unused
            ,SingleToMultiBlockRatio]).


do_gc() ->
    achlys:gc().

gc_info() ->
    statistics(garbage_collection).

read_temp() ->
    pmod_nav:read(acc , [out_temp]).


create_table(Name) ->
    case ets:info(Name, size) of
      undefined ->
        ets:new(Name , [
            ordered_set
            , public
            , named_table
            , {heir , whereis(achlys_sup) , []}
        ]);
      _ ->
        % TODO : check for existing table with ownership at achlys_sup PID
        % and transfer if possible
        Name
    end.

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

forge_binary_packet(Size) ->
    <<1:Size>>.

bitstring_name() ->
    atom_to_binary(node() , utf8).

get_inet_least_significant() ->
  {ok, [{{_,_,_,In},_,_}|_T]} = inet:getif(),
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

get_variable_identifier(Name) ->
    {unicode:characters_to_binary([erlang:atom_to_binary(node(),utf8)
        , "_"
        , erlang:atom_to_binary(Name,utf8)] , utf8) ,
        unicode:characters_to_binary([erlang:atom_to_binary(node(),utf8)
            , "_"
            , erlang:atom_to_binary(Name,utf8), "_size"] , utf8)}.

get_pressure_identifier() ->
    unicode:characters_to_binary([erlang:atom_to_binary(node(),utf8),"_press"],utf8).

get_light_identifier() ->
    unicode:characters_to_binary([erlang:atom_to_binary(node(),utf8),"_light"],utf8).

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

rainbow_test() ->
    achlys:bite(achlys:declare(rainbow
        ,all
        ,permanent
        ,achlys_task_container:rainbow())).

add_node(Node) when is_atom(Node) ->
    achlys_config:set(boards, [Node] ++ achlys_config:get(boards, [])).


%%====================================================================
%% NOTE: Binary encoding of values propagated through CRDTs instead
%% of propagating tuples directly in the cluster
%%
%% 32 bits :
%%
%% first 8 bits :
%% [_][_] [_][_]  [_][_] [_][_]
%%                |--------------> 4 bits for node ID : [0][0] [0][0]
%%                |--------------> 4 bits for node ID : [0][0] [0][1]
%%                                                          ...
%%                |--------------> 4 bits for node ID : [1][1] [1][1]
%%        |----|---------> 2 bits for value type : [0][0] => temperature
%%        |----|---------> 2 bits for value type : [0][1] => pressure
%%        |----|---------> 2 bits for value type : [1][0] => light
%%        |----|---------> 2 bits for value type : [1][1] => humidity
%%
%% 8-16 bits :
%% [_][_] [_][_]  [_][_] [_][_]
%%        |-------------> 8 bits for aggregations count : 0 to 255
%%
%% 14-32 bits :
%% [_][_] [_][_]  [_][_] [_][_]  [_][_] [_][_]  [_][_]
%% |-------------> 14 bits for aggregation value : 0 to 262 143
%%====================================================================