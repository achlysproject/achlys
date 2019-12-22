%%%-------------------------------------------------------------------
%%% @author Igor Kopestenski <igor.kopestenski@uclouvain.be>
%%%     [https://github.com/achlysproject/achlys]
%%% @doc
%%% Container module .
%%% @end
%%% Created : 21. Dec 2019 17:00
%%%-------------------------------------------------------------------
-module(achlys_task_container).
-author("Igor Kopestenski <igor.kopestenski@uclouvain.be>").

-compile({nowarn_export_all}).
-compile(export_all).

%%====================================================================
%% Sample Task Model Functions
%% for testing and demonstration purposes.
%%====================================================================
-export([rainbow/0]).
-export([light/0]).
-export([mintemp/0]).
%%====================================================================


%%====================================================================
%% Task model functions
%%====================================================================

-spec rainbow() -> erlang:function().
rainbow() ->
    fun() ->
        Random = fun() ->
            {rand:uniform(2) - 1, rand:uniform(2) -1, rand:uniform(2) - 1}
        end,
        _ = [grisp_led:pattern(L, [{100, Random}]) || L <- [1,2]]
    end.

-spec light() -> erlang:function().
light() ->
    fun() ->
            AmbLight = pmod_als:percentage(),
            logger:log(notice , "AL Level : ~p % ~n", [AmbLight]),
            AmbLight
    end.

-spec mintemp() -> erlang:function().
mintemp() ->
    fun() ->
        Id = {<<"temp">>, state_gset},
        {ok, {_, _, _, _}} = lasp:declare(Id, state_gset),
        L = lists:foldl(fun
            (_Elem, AccIn) -> timer:sleep(5000),
                Temp = pmod_nav:read(acc, [out_temp]),
                Temp ++ AccIn
        end, [], lists:seq(1,5)),
        SList = lists:usort(L),
        Min = hd(SList),
        Name = node(),
        lasp:update(Id, {add, {Min, Name}}, self()),
        spawn(fun() ->
                lasp:read(Id, {cardinality, 5}),
                {ok, S} = lasp:query(Id),
                Fetched = sets:to_list(S),
                {_Minimum, Node} = hd(lists:usort(Fetched)),
                Self = node(),
                case Node =:= Self of
                    true ->
                        [ grisp_led:color(X, blue) || X <- [1,2] ];
                    _ ->
                        [ grisp_led:color(X, red) || X <- [1,2] ]
                end
        end)
    end.