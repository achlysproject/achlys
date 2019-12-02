%%%-------------------------------------------------------------------
%%% @author Igor Kopestenski <igor.kopestenski@uclouvain.be>
%%%     [https://github.com/achlysproject/achlys]
%%%
%%% @doc
%%% This is a utility module that provides shortcuts
%%% for operations on achlys application environment variables.
%%% @end
%%% Created : 06. Nov 2018 21:38
%%%-------------------------------------------------------------------
-module(achlys_config).
-author("Igor Kopestenski <igor.kopestenski@uclouvain.be>").

%% API
-export([get/1]).
-export([get/2]).
-export([set/2]).

%% @doc
%% Retrieve the value of a configuration parameter
%% of the achlys application. If the parameter is
%% not present in the configuration, the atom
%% `undefined' is returned.
%% @equiv application:get_env(achlys, Key)
-spec get(atom()) -> undefined|{ok , term()}.
get(Key) ->
    application:get_env(achlys , Key).

%% @doc
%% Retrieve the value of a configuration parameter
%% of the achlys application, and return a default
%% value if the parameter is not present.
%% @equiv application:get_env(achlys, Key, Default)
-spec get(atom() , term()) -> term().
get(Key , Default) ->
    %% NOTE : get/2 was initially wrongly success typed with {ok, term()}
    %% but it will obviously always return the retrieved value or the default
    %% hence it never fails, even if the process does not exist.
    %% The typing is simply any term
    application:get_env(achlys , Key , Default).

%% @doc
%% Sets a parameter in the achlys application configuration
%% to the given value.
%% @equiv application:set_env(achlys, Par, Val)
-spec set(atom() , term()) -> ok.
set(Par , Val) ->
    application:set_env(achlys , Par , Val).
