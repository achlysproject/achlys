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
%% Task model functions
%%====================================================================

%% @doc Returns the task model variable based on the given arguments
%% in the form of a map.
-spec declare(Name::atom()
    , Targets::[node()] | all
    , ExecType::single | permanent
    , Func::function()) -> task() | erlang:exception().
declare(Name, Targets, ExecType, Func) ->
    try lists:member(ExecType, [single, permanent]) of
        true when Targets =:= all orelse is_list(Targets) ->
            form_map(Name, Targets, ExecType, Func)
    catch
        Exception:Reason -> {caught, Exception, Reason}
    end.

form_map(Name, Targets, ExecType, Func) ->
        #{name => Name
        , targets => task_flag(Targets)
        , execution_type => task_flag(ExecType)
        , function => Func}.

task_flag(all) ->
    <<0>>;
task_flag(permanent) ->
    <<0>>;
task_flag(single) ->
    <<1>>;
task_flag(Args) ->
    Args.

-spec rainbow() -> erlang:function().
rainbow() ->
    Func = fun() ->
        Random = fun() ->
            {rand:uniform(2) - 1, rand:uniform(2) -1, rand:uniform(2) - 1}
        end,
        _ = [grisp_led:pattern(L, [{100, Random}]) || L <- [1,2]]
    end,
    Func.

ledtask() ->
    Func = fun
        () ->
            logger:log(notice, "Executing ledtask "),
            grisp_led:color(1, blue),
            grisp_led:color(1, red),
            grisp_led:color(1, blue)
    end.

get_pmod_nav_temp_task() ->
    F = fun
        () ->
            logger:log(notice, "Executing pmod_nav_temp_task "),
            achlys:venom()
            % achlys:venom(),
            % {TempId, _} = achlys_util:get_variable_identifier(temperature),
            % InvariantFun = fun
            %     % (TempId) ->
            %     () ->
            %         logger:log(notice, "Enforcing func for Id : ~p", [TempId]),
            %         % lasp:read(Id, {cardinality, 3}),
            %         ok = achlys_pmod_als_worker:terminate(normal, #{})
            %         % L = achlys_util:query(Id),
            %         % io:format("Temperatures : ~p", [L])
            % end,
            % % spawn(F(TempId))
            % lasp:invariant(TempId, {cardinality, 3}, InvariantFun)
    end,
    #{name => pmod_nav_temp_task
    , targets => ?TARGET_ALL_NODES
    , execution_type => ?SINGLE_EXECUTION_TASK
    , function => F}.

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

do_gc() ->
    _ = [erlang:garbage_collect(X , [{type , 'major'}]) 
        || X <- erlang:processes()] ,
    ok.

gc_info() ->
    statistics(garbage_collection).

read_temp() ->
    pmod_nav:read(acc , [out_temp]).


create_table(Name) ->
    case ets:info(Name, size) of
      undefined ->
        T = ets:new(Name , [
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
    achlys:bite(achlys:declare(t,all,permanent,fun() -> achlys:rainbow() end)).

add_node(Node) when is_atom(Node) ->
    achlys_config:set(boards, [Node] ++ achlys_config:get(boards, [])).


%%====================================================================
%% TODO: Binary encoding of values propagated through CRDTs instead
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