-module(achlys_util).
-author("Igor Kopestenski <igor.kopestenski@uclouvain.be>").

-include("achlys.hrl").

-compile({nowarn_export_all}).
-compile(export_all).

%%====================================================================
%% Utility functions
%%====================================================================

query(Id) ->
  {ok, S} = lasp:query(Id),
  sets:to_list(S).

query(Name, Type) ->
  BitString = atom_to_binary(Name,utf8),
  {ok, S} = lasp:query({BitString, Type}),
  sets:to_list(S).

declare_crdt(Name, Type) ->
  Bitstring = atom_to_binary(Name,utf8),
  {ok, {Id, _, _, _}} = lasp:declare({Bitstring, Type}, Type),
  Id.

do_gc() ->
  _ = [ erlang:garbage_collect(X, [{type, 'major'}]) || X <- erlang:processes() ],
  ok.

%% @doc Returns information about garbage collection, for example:
%%
%% > statistics(garbage_collection).
%% {85,23961,0}
%%
%% This information can be invalid for some implementations.
gc_info() ->
  statistics(garbage_collection).

get_temp() ->
  pmod_nav:read(acc, [out_temp]).

%% @todo reduce interleavings between modules
%% and perform function calls without shortcuts in @module
table(Name) ->
  ets:new(Name, [ordered_set,
                named_table,
                public,
                {heir, whereis(achlys_sup), []}]).

insert_timed_key(TableName, Value) ->
  ets:insert(TableName, {erlang:monotonic_time(),Value}).

store_temp() ->
  ets:insert(temp, {erlang:monotonic_time(),pmod_nav:read(acc, [out_temp])}).

grisp() ->
  application:ensure_all_started(grisp).

time() ->
  logger:log(notice, "Time : ~p ~n",[erlang:monotonic_time()]).

recon() ->
    % L = recon_alloc:sbcs_to_mbcs(current),
    [ io:format("sbcs_to_mbcs === ~p ~n", [X]) || X <- recon_alloc:sbcs_to_mbcs(current) ],
    recon(avg).
recon(avg) ->
    [ io:format("average_block_sizes === ~p ~n", [X]) || X <- recon_alloc:average_block_sizes(current) ],
    recon(usage);
recon(usage) ->
    io:format("memory === ~p ~n", [recon_alloc:memory(usage,current)]),
    recon(cache);
recon(cache) ->
    recon_alloc:cache_hit_rates().

maxrecon() ->
    % L = recon_alloc:sbcs_to_mbcs(current),
    [ io:format("sbcs_to_mbcs === ~p ~n", [X]) || X <- recon_alloc:sbcs_to_mbcs(current) ],
    maxrecon(avg).
maxrecon(avg) ->
    [ io:format("average_block_sizes === ~p ~n", [X]) || X <- recon_alloc:average_block_sizes(current) ],
    maxrecon(usage);
maxrecon(usage) ->
    io:format("memory === ~p ~n", [recon_alloc:memory(usage,current)]),
    maxrecon(cache);
maxrecon(cache) ->
    recon_alloc:cache_hit_rates().


%% Stress intensity
%% achlys_util:stress_throughput().
% stress_throughput() ->
%     stress_throughput(achlys_config:get(packet_config)).
% stress_throughput(#{stress_delay    := Interval,
%                 operations_count     := Count,
%                 packet_size          := Size}) ->
%     ((Count * Size) * (Interval / (?MILLION))). %% returns float
%     % (Count * Size) div ?MILLION.

fetch_resolv_conf() ->

    {ok, F} = case os:type() of
        {unix,rtems} ->
          {ok, "nofile"};
        _ ->
          {ok, Cwd} = file:get_cwd(),
          Wc = filename:join(Cwd,"**/files/"),
          filelib:find_file("erl_inetrc", Wc)
    end,
    ok = inet_db:add_rc(F),
    inet_db:get_rc().

% foo(N, Bin) ->
%    <<X:N,T/binary>> = Bin,
%    {X,T}.
% <<Packet:Size/bitstring>> = <<1:Size>>.
get_packet(Size) ->
    <<1:Size>>.

bitstring_name() ->
  atom_to_binary(node(),utf8).

declare_awset(Name) ->
  String = atom_to_list(Name),
  AWName = list_to_bitstring(String),
  AWSetType = state_awset,
  {ok, {AWSet, _, _, _}} = lasp:declare({AWName, AWSetType}, AWSetType),
  application:set_env(achlys, awset, AWSet),
  AWSet.

%%====================================================================
%% Clustering functions
%%====================================================================

remotes_to_atoms([H|T]) ->
    C = unicode:characters_to_list(["achlys@",H]),
    R = list_to_atom(C),
    [R|remotes_to_atoms(T)];
remotes_to_atoms([]) ->
    [].

binary_remotes_to_atoms([H|T]) ->
    [binary_to_atom(H,utf8)|binary_remotes_to_atoms(T)];
binary_remotes_to_atoms([]) ->
    [].

seek_neighbors() ->
    Rc = inet_db:get_rc(),
    seek_neighbors(Rc).
seek_neighbors([{host,_Addr,N}|T]) ->
    [list_to_bitstring(["achlys@",N])|seek_neighbors(T)];
seek_neighbors([{_Arg,_Val}|T]) ->
    seek_neighbors(T);
seek_neighbors([]) ->
    [].

join(Host) ->
  Manager = rpc:call(Host, partisan_peer_service, manager, []),
  case Manager of
    partisan_hyparview_peer_service_manager ->
      Node = rpc:call(Host, Manager, myself, []),
      ok = partisan_peer_service:join(Node),
      logger:log(info, "Joined ~p~n", [Host]),
      Node;
    {badrpc, Reason} ->
      logger:log(error, "Unable to RPC remote : ~p~n", [Reason]),
      {error, Reason};
    {error, Reason} ->
      logger:log(error, "Unable to retrieve remote : ~p~n", [Manager]),
      {error, Reason}
  end.

clusterize() ->
    N = seek_neighbors(),
    Remotes = binary_remotes_to_atoms(N),
    Self = node(),
    clusterize(Remotes,Self).

clusterize([H|Remotes],Self) ->
    case H =/= Self of
        true ->
            Res = achlys_util:join(H),
            % Res = achlys_util:fakejoin(H),
            [Res|clusterize(Remotes,Self)];
        _ ->
            [clusterize(Remotes,Self)]
    end;

clusterize([],_Self) ->
    [].
