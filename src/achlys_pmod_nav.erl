%%%-------------------------------------------------------------------
%% @author Igor Kopestenski <i.kopest@gmail.com>
%%   [https://github.com/Laymer/achlys/]
%% @doc This is a Pmod_NAV worker server.
%% @end
%%%-------------------------------------------------------------------

-module(achlys_temp).

-behaviour(gen_server).

-include("achlys.hrl").

%% API
-export([start_link/1]).
-export([create_table/1]).
-export([declare_crdt/2]).
-export([store_temp/0]).
-export([get_mean/0]).
-export([run/0]).

%% Gen Server Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         handle_continue/2,
         terminate/2,
         code_change/3]).

%%====================================================================
%% Macros
%%====================================================================

-define(MACRO_TEMPLATE(X), {X} ).

%%====================================================================
%% Records
%%====================================================================

-record(state, {
  crdt :: any(),
  gc_interval :: pos_integer(),
  poll_interval :: pos_integer(),
  mean_interval :: pos_integer(),
  table :: any()
}).

%%====================================================================
%% API
%%====================================================================

start_link(NavConfig) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, NavConfig, []).

create_table(Name) ->
    gen_server:call(?MODULE, {table, Name}).

declare_crdt(Name, Type) ->
    gen_server:call(?MODULE, {declare, Name, Type}).
% achlys_temp:store_temp().
store_temp() ->
    gen_server:call(?MODULE, store_temp).

run() ->
    gen_server:call(?MODULE, run).

get_mean() ->
    gen_server:cast(?MODULE, mean).

%%====================================================================
%% Gen Server Callbacks
%%====================================================================

init(NavConfig) ->
    %% For test purposes. Grisp_app allows calls to emulated pmod_nav.
    {ok, _} = application:ensure_all_started(grisp),
    T = ets:new(maps:get(table, NavConfig), [ordered_set, public, named_table]),
    PollInterval = maps:get(poll_interval, NavConfig),
    {ok, #state{gc_interval = maps:get(gc_interval, NavConfig)
        , poll_interval = PollInterval
        , mean_interval = (PollInterval * 10)
        , table = T}}.
    % {ok, #state{temp = [], table = }}.

%%--------------------------------------------------------------------

handle_call(run, _From, State) ->
    logger:log(notice, "DECLARED ~n"),
    Id = achlys_util:declare_crdt(temp, state_awset),
    achlys_util:insert_timed_key(State#state.table, pmod_nav:read(acc, [out_temp])),
    erlang:send_after(State#state.poll_interval,self(),poll),
    erlang:send_after(State#state.gc_interval,self(),gc),
    erlang:send_after(State#state.mean_interval,self(),mean),
    {reply, ok, State#state{crdt = Id}};

handle_call(store_temp, _From, State) ->
    achlys_util:insert_timed_key(State#state.table, pmod_nav:read(acc, [out_temp])),
    % achlys_util:insert_timed_key(State#state.table, get_temp()).
    {reply, ok, State};

%%--------------------------------------------------------------------

handle_call({table, Name}, _From, State) ->
    T = achlys_util:table(Name),
    {reply, ok, State#state{table = T}};

%%--------------------------------------------------------------------

handle_call({declare, Name, Type}, _From, State) ->
    logger:log(notice, "DECLARED ~n"),
    Id = achlys_util:declare_crdt(Name, Type),
    {reply, ok, State#state{crdt = Id}};

%%--------------------------------------------------------------------

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%%--------------------------------------------------------------------

handle_cast(mean, State) ->
    Mean = get_temp_mean(State#state.table),
    lasp:update(State#state.crdt, {add, Mean}, self()),
    {noreply, State};

%%--------------------------------------------------------------------

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------

handle_info(poll, State) ->
    achlys_util:insert_timed_key(State#state.table, pmod_nav:read(acc, [out_temp])),
    erlang:send_after(State#state.poll_interval,self(),poll),
    {noreply, State};

%%--------------------------------------------------------------------

handle_info(gc, State) ->
    lasp:update(State#state.crdt, {add, get_temp_mean(State#state.table)}, self()),
    achlys_util:gc(),
    erlang:send_after(State#state.gc_interval,self(),gc),
    {noreply, State};

%%--------------------------------------------------------------------

handle_info(mean, State) ->
    Mean = get_temp_mean(State#state.table),
    lasp:update(State#state.crdt, {add, Mean}, self()),
    {noreply, State};

%%--------------------------------------------------------------------

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------

%% This function is called by a gen_server process
%% whenever a previous callback returns {continue, Continue}.
%% handle_continue/2 is invoked immediately after the previous callback,
%% which makes it useful for performing work after initialization
%% or for splitting the work in a callback in multiple steps,
%% updating the process state along the way.

%%--------------------------------------------------------------------

handle_continue(_Continue, State) ->
  % {noreply,NewState} | {noreply,NewState,Timeout}
  % | {noreply,NewState,hibernate}
  % | {noreply,NewState,{continue,Continue}}
  % | {stop,Reason,NewState}
    {noreply, State}.

%%--------------------------------------------------------------------

terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

get_temp_mean(Tab) ->
    Sum = ets:foldl(fun
      (Elem, AccIn) ->
        {_, [Temp]} = Elem,
        Temp + AccIn
    end, 0, Tab),
    Len = ets:info(Tab, size),
    {Len, (Sum / Len)}.
