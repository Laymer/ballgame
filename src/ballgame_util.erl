-module(ballgame_util).

-include("ballgame.hrl").
-compile({nowarn_export_all}).

-compile(export_all).

%%====================================================================
%% Utility functions
%%====================================================================

temp_aggregates_crdt() ->
  {ok, S} = lasp:query({<<"temp">>, state_awset}),
  sets:to_list(S).

declare_crdt(Name, Type) ->
  Bitstring = atom_to_binary(Name,utf8),
  {ok, {Id, _, _, _}} = lasp:declare({Bitstring, Type}, Type),
  Id.

table(Name) ->
  ets:new(Name, [ordered_set,
                named_table,
                public,
                {heir, whereis(ballgame_matchmaker), []}]).
temp() ->
  pmod_nav:read(acc, [out_temp]).

insert_timed_key(TableName, Value) ->
  ets:insert(TableName, {erlang:monotonic_time(),Value}).

store_temp() ->
  ets:insert(temp, {erlang:monotonic_time(),pmod_nav:read(acc, [out_temp])}).

grisp() ->
  application:ensure_all_started(grisp).

gc() ->
  _ = [ erlang:garbage_collect(X, [{type, 'major'}]) || X <- erlang:processes() ].

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


%% Stress intensity in Kbps
% ballgame_util:stress_throughput().
stress_throughput() ->
    stress_throughput(ballgame_util:get(packet_config)).
stress_throughput(#{stress_delay    := Interval,
                operations_count     := Count,
                packet_size          := Size}) ->
    % ((Count * Size) * (Interval / (?MILLION))). %% returns float
    (Count * Size) div ?MILLION.

fetch_resolv_conf() ->
    {ok, F} = case is_shell() of
        true ->
            {ok, Cwd} = file:get_cwd(),
            Wc = filename:join(Cwd,"**/files/"),
            filelib:find_file("erl_inetrc", Wc);
        _ ->
            {ok, "nofile"}
    end,
    ok = inet_db:add_rc(F),
    inet_db:get_rc().

myname() ->
    {ok, N} = inet:gethostname(),
    N.

team(Players) ->
  [ name(X) || X <- Players ].

name(Host) when is_integer(Host) ->
  ?PLAYER(Host);
name(Host) when is_atom(Host) ->
  list_to_atom(unicode:characters_to_list(["ballgame@", atom_to_list(Host)], utf8));
name(Host) when is_list(Host) ->
  [list_to_atom(unicode:characters_to_list(["ballgame@", atom_to_list(X)], utf8)) || X <- lists:flatten(Host)].

is_shell() ->
  case os:type() of
    {unix, rtems} -> false;
    _ -> true
  end.

get(Key) ->
    {ok, Value} = application:get_env(ballgame, Key), Value.

get(Key, Default) -> application:get_env(ballgame, Key, Default).

mgr() ->
  partisan_peer_service:manager().

me() ->
  (mgr()):myself().

members() ->
  {ok, Members} = partisan_peer_service:members(),
  Members.

alone() ->
    length(members() -- [node()]) == 0.

% foo(N, Bin) ->
%    <<X:N,T/binary>> = Bin,
%    {X,T}.
% <<Packet:Size/bitstring>> = <<1:Size>>.
get_packet(Size) ->
    <<1:Size>>.

bitstring_name() ->
    N = node(),
    bitstring_name(N).
bitstring_name(N) ->
    atom_to_binary(N,utf8).

declare_awset(Name) ->
  String = atom_to_list(Name),
  AWName = list_to_bitstring(String),
  AWSetType = state_awset,
  {ok, {AWSet, _, _, _}} = lasp:declare({AWName, AWSetType}, AWSetType),
  application:set_env(ballgame, awset, AWSet),
  AWSet.


%%====================================================================
%% Clustering functions
%%====================================================================

-ifdef(SHELL).
-define(GET_RC(), fetch_resolv_conf()).
-else.
-define(GET_RC(), inet_db:get_rc()).
-endif.

%%====================================================================
%% NO LOGGING
%%====================================================================
-ifdef(GRISP).

remotes_to_atoms([H|T]) ->
    C = unicode:characters_to_list(["ballgame@",H]),
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
    [list_to_bitstring(["ballgame@",N])|seek_neighbors(T)];
seek_neighbors([{_Arg,_Val}|T]) ->
    seek_neighbors(T);
seek_neighbors([]) ->
    [].

old_seek() ->
    lists:filtermap(fun
    (Tup) ->
        case Tup of
        {host,_Addr,[Hostname]} ->

          {true, list_to_bitstring(["ballgame@",Hostname])};
        _ ->
          false
        end
    end, inet_db:get_rc()).

fakejoin(Host) ->
    timer:sleep(rand:uniform(50)),
    partisan_hyparview_peer_service_manager:myself().

bin_fakejoin(Host) ->
    binary_to_atom(Host,utf8),
    timer:sleep(rand:uniform(50)),
    partisan_hyparview_peer_service_manager:myself().

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
            Res = ballgame_util:join(H),
            % Res = ballgame_util:fakejoin(H),
            [Res|clusterize(Remotes,Self)];
        _ ->
            [clusterize(Remotes,Self)]
    end;

clusterize([],_Self) ->
    [].

-else.
%%====================================================================
%% LOGGING
%%====================================================================

seek_neighbors() ->
  logger:log(info, "Pinging possible neighbors ~n"),
  Rc = ?GET_RC(),
  MyName = myname(),
  Hosts = lists:filtermap(fun
    (Tup) ->
      case Tup of
        {host,_Addr,[Hostname]} when MyName =/= Hostname ->
          {true, list_to_atom("ballgame@" ++ Hostname)};
        _ ->
          false
      end
  end, Rc),
  net_adm:ping_list(Hosts).

join(Host) ->
  Manager = rpc:call(Host, partisan_peer_service, manager, []),
  case Manager of
    partisan_hyparview_peer_service_manager ->
      Node = rpc:call(Host, Manager, myself, []),
      ok = partisan_peer_service:join(Node),
      logger:log(info, "Joined ~p~n", [Host]),
      Node;
    {error, Reason} ->
      logger:log(error, "Unable to retrieve remote : ~p~n", [Manager]),
      {error, Reason}
  end.

clusterize() ->
    logger:log(info, "Joining reachable nodes ~n"),
    L = [ ballgame_util:join(X) ||
      X <- seek_neighbors(),
      X =/= node() ],
    logger:log(info, "Joined = ~p ~n", [L]),
    L.

-endif.


% join(Host) ->
%     Manager = rpc:call(Host, partisan_peer_service, manager, []),
%     %% TODO : separate in funcs
%     case Manager of
%       partisan_hyparview_peer_service_manager ->
%         Node = rpc:call(Host, Manager, myself, []),
%         ok = partisan_peer_service:join(Node),
%         Node;
%       {badrpc, Reason} ->
%         logger:log(error, "Unable to RPC remote : ~p~n", [Reason]),
%         [];
% join(Host,{error, Reason})->
%         logger:log(error, "Error : ~p~n", [Reason]),
%         [];
% join(Host,{error, unkown})->
%         logger:log(error, "Error : ~p~n", [unkown]),
%         [].


% old_clusterize() ->
%     [ ballgame_util:join(X) ||
%         X <- old_seek(),
%         X =/= atom_to_binary(node(),utf8) ].
        % X =/= node() ].
    % T2 = erlang:monotonic_time() - T1,
    % logger:log(notice, "Old clusterize : ~p~n", [T2]),
    % L.
  % logger:log(info, "Joined = ~p ~n", [L]).



% X <- Remotes(),
% X =/= node() ].


% T1 = erlang:monotonic_time(),
% L = [ ballgame_util:join(X) ||
% L = [ ballgame_util:fakejoin(X) ||
%     X <- old_seek(),
%     % X =/= atom_to_binary(node(),utf8) ],
%     X =/= node() ],



% T1 = erlang:monotonic_time(),
% L = net_adm:ping_list(lists:filtermap(fun
% (Tup) ->
%     case Tup of
%     {host,_Addr,[Hostname]} ->
%       {true, list_to_atom("ballgame@" ++ Hostname)};
%     _ ->
%       false
%     end
% end, inet_db:get_rc())),
% L = lists:filtermap(fun
% (Tup) ->
%     case Tup of
%     {host,_Addr,[Hostname]} ->
%       {true, list_to_atom("ballgame@" ++ Hostname)};
%       % {true, Hostname};
%     _ ->
%       false
%     end
% end, inet_db:get_rc()),
% T2 = erlang:monotonic_time() - T1,
% logger:log(notice, "Old seek time : ~p~n", [T2]),
% L.
% << << (X*2) >> || <<X>> <= <<1,2,3>> >>.

% remotes_to_atoms(L) -> sum(L, []).
%
% map_pairs2(_Map, [], Ys) ->
%     Ys;
% map_pairs2(_Map, [_|_]=Xs, [] ) ->
%     Xs;
% map_pairs2(Map, [X|Xs], [Y|Ys]) ->
%     [Map(X, Y)|map_pairs2(Map, Xs, Ys)].



% {ok, {AWSet1, _, _, _}} = lasp:update(AWSet, {add, AWSetVal1}, self()).
% maybe_get_remote() ->
%     MList = members(),
%     maybe_get_remote(MList).
% maybe_get_remote(Mlist) when is_list(MList) and length(MList) == 1 ->
%     N = hd(MList),
%     maybe_get_remote(N).
% maybe_get_remote()
%
%
%   try hd(List) of
%     Hd ->
%       Hd
%   catch
%     badarg:T ->
%       {badarg,T}
% end.


% main() ->
%     L = lists:seq(1,100000),
%     % F = fun(X) -> X end,
%     spawn(fun() -> io:format("New : ~p~n", [element(1, timer:tc(?MODULE, new_time, [L]))]) end),
%     spawn(fun() -> io:format("Old : ~p~n", [element(1, timer:tc(?MODULE, old_time, [L]))]) end).
%
% tail_map(F, L) ->
%     tail_map(F, L, []).
%
% tail_map(_, [], Acc) -> lists:reverse(Acc);
% tail_map(F, [H|T], Acc) -> tail_map(F, T, [F(H)|Acc]).
%
% body_map(_, []) -> [];
% body_map(F, [H|T]) -> [F(H) | body_map(F, T)].


% new_time(L) ->
%     % L = lists:seq(1,100),
%     % F = fun(X) -> X end,
%     Times = [ element(1, timer:tc(?MODULE, clusterize, []) ) || X <- L],
%     lists:sum(Times).
%
% old_time(L) ->
%     % L = lists:seq(1,100),
%     % F = fun(X) -> X end,
%     Times = [ element(1, timer:tc(?MODULE, old_clusterize, []) ) || X <- L],
%     lists:sum(Times).
    % T = element(1, timer:tc(?MODULE, clusterize, []) ),
    % spawn(fun() -> io:format("Old:~p~n", [element(1, timer:tc(?MODULE, m_tail_map, []))]) end),
    % spawn(fun() ->
        % io:format("New:~p~n", [element(1, timer:tc(?MODULE, clusterize, []) )])
    % end).
%
% m_tail_map(_, _, 0) -> ok;
% m_tail_map(F, L, N) ->
%     tail_map(F,L),
%     m_tail_map(F, L, N-1).
%
% m_body_map(_, _, 0) -> ok;
% m_body_map(F, L, N) ->
%     body_map(F,L),
%     m_body_map(F, L, N-1).
%
% remotes_to_bin([H|T]) ->
    % remote
% remotes_to_bin([H|T]) ->
%     C = unicode:characters_to_list(["ballgame@",H]),
%     R = list_to_atom(C),
%     [R|remotes_to_bin(T)];
% remotes_to_bin([]) ->
%     [].



% case is_shell() of
%     true ->
%         % {ok,_} = application:ensure_all_started(inet_db),
%         {ok, Cwd} = file:get_cwd(),
%         Wc = filename:join(Cwd,"**/files/"),
%         {ok, Rc} = filelib:find_file("erl_inetrc", Wc),
%         ok = inet_db:add_rc(Rc);
%     _ ->
%         error
% end,

% {ok, F} = case is_shell() of
%     true ->
%         {ok, Cwd} = file:get_cwd(),
%         Wc = filename:join(Cwd,"**/files/"),
%         filelib:find_file("erl_inetrc", Wc);
%     _ ->
%         {error, "nofile"}
% end,
% ok = inet_db:add_rc(F),
% Rc = inet_db:get_rc(),

% {host,_Addr,[Hostname]} ->
  % [Sname|_] = string:split(atom_to_binary(node(),utf8),"@"),
  % Str = unicode:characters_to_list([Sname,"@",Hostname],utf8),
  % N = list_to_atom("ballgame@" ++ Hostname),


    % {[Numbers],Team} = case ballgame_util:get(testing) of
    %     true ->
    %         ?FAKETEAM(ballgame_util:get(fakeplayers));
    %     false ->
    %         ?TEAM(ballgame_util:get(players));
    %
    % end™ı
    % {ok, Names} = net_adm:names(),

%% NOTE : see link below
% https://github.com/erlang/otp/blob/cedccd3e8d42189b91d46c2637b9ce39675318f4/lib/kernel/src/net_adm.erl#L114
    % [Players] = ballgame_util:get(players),
    % Team = team(Players),
    % _L = [ ballgame_util:join(X) ||
    % net_adm:ping(X) =:= pong ],
    % M = ballgame_util:members(),
    % logger:log(info, "Joined = ~p ~n", [M]),




  %% A common situation in "life" is to have a configuration file with a list
  %% of nodes, and then at startup, all nodes in the list are ping'ed
  %% this can lead to no end of troubles if two disconnected nodes
  %% simultaneously ping each other.
  %% Use this function in order to do it safely.
  %% It assumes a working global.erl which ensures a fully
  %% connected network.
  %% Had the erlang runtime system been able to fully cope with
  %% the possibility of two simultaneous (unix) connects, this function would
  %% merley  be lists:map({net_adm, ping}, [], Nodelist).
  %% It is also assumed, that the same (identical) Nodelist is given to all
  %% nodes which are to perform this call (possibly simultaneously).
  %% Even this code has a flaw, and that is the case where two
  %% nodes simultaneously and without *any* other already
  %% running nodes execute this code. :-(

  %% -spec ping_list([atom()]) -> [atom()].

    % [list_to_atom(unicode:characters_to_list(["ballgame@", atom_to_list(X)], utf8)) || X <- Host].
    % ?FAKEPLAYER(Host).
    % list_to_atom(unicode:characters_to_list(["ballgame@", Host], utf8)).

%     lists:filtermap(fun(X) -> case X rem 2 of 0 -> {true, X div 2}; _ -> false end end, [1,2,3,4,5]).
% [1,2]
 %    [{host,{169,254,16,1},["my_grisp_board_1"]},
 % {host,Addr,[Hostname]} = {host,{169,254,16,1},["my_grisp_board_1"]}.
 % {host,{169,254,16,2},["my_grisp_board_2"]},
 % {host,{169,254,16,3},["my_grisp_board_3"]},
 % {host,{169,254,16,4},["my_grisp_board_4"]},
 % {host,{169,254,16,5},["my_grisp_board_5"]},
 % {host,{169,254,16,6},["my_grisp_board_6"]},
 % {host,{169,254,16,7},["my_grisp_board_7"]},
 % {host,{169,254,16,8},["my_grisp_board_8"]},
 % {host,{169,254,16,9},["my_grisp_board_9"]},
 % {host,{169,254,16,10},["my_grisp_board_10"]},
 % {host,{169,254,16,11},["my_grisp_board_11"]},
 % {host,{169,254,16,12},["my_grisp_board_12"]},
 % {host,{169,254,187,89},["laymer"]},
 % {host,{169,254,187,90},["laymer-3"]},
 % {resolv_conf,"/etc/resolv.conf"},
 % {hosts_file,[]},
 % {cache_size,0},
 % {lookup,[file,native]}] = inet_db:get_rc().
    % {ok,[{"ballgame",58322},
    %      {"ballgame2",58332},
    %      {"ballgame3",58383},
    %      {"ballgame4",58413}]} = net_adm:names().

% -ifdef(debug).
% -define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE,?LINE,X])).
% -else.
% -define(LOG(X), true).
% -endif.

% -if(?OTP_RELEASE >= 22).
%   %% Code that will work in OTP 22 or higher
% -elif(?OTP_RELEASE >= 21).
%   %% Code that will work in OTP 21 or higher
% -endif.

%% TRACING LIST :
% [ballgame,ballgame_app,ballgame_matchmaker,ballgame_sup,
%  ballgame_util,player,partisan,partisan_app,partisan_sup,
%  partisan_pool,partisan_pool_sup,
%  partisan_acknowledgement_backend,partisan_peer_service,
%  partisan_hyparview_peer_service_manager,
%  partisan_plumtree_backend,
%  partisan_rpc_backend].
% [ballgame,ballgame_app,ballgame_matchmaker,ballgame_sup,
%  ballgame_util,player,partisan,partisan_app,partisan_sup,
%  partisan_pool,partisan_pool_sup,
%  partisan_hyparview_peer_service_manager].
% grapherl:modules("/home/laymer/EdgeComputing/ballgame/_build/test/lib/partisan/ebin","partisan", [partisan_acknowledgement_backend, partisan_causality_backend, partisan_client_server_peer_service_manager, partisan_default_peer_service_manager, partisan_hyparview_xbot_peer_service_manager, partisan_peer_service_client, partisan_peer_service_console, partisan_peer_service_events, partisan_peer_service_manager, partisan_plumtree_util, partisan_promise_backend, partisan_rpc_backend, partisan_static_peer_service_manager, partisan_transform,partisan_transformed_module, partisan_util,partisan_vclock]).

    % logger:log(notice, "sbcs_to_mbcs : current ~p ~n", [recon_alloc:sbcs_to_mbcs(current)]),
    % logger:log(notice, "sbcs_to_mbcs : max ~p ~n", [recon_alloc:sbcs_to_mbcs(max)]),
    % logger:log(notice, "average_block_sizes : current ~p ~n", [recon_alloc:average_block_sizes(current)]),
    % logger:log(notice, "average_block_sizes : max ~p ~n", [recon_alloc:average_block_sizes(max)]),
    % % logger:log(notice, "fragmentation : current ~p ~n", [recon_alloc:fragmentation(current)]),
    % % logger:log(notice, "fragmentation : max ~p ~n", [recon_alloc:fragmentation(max)]),
    % logger:log(notice, "Usage : current ~p ~n", [recon_alloc:memory(usage,current)]),
    % logger:log(notice, "fragmentation : max ~p ~n", [recon_alloc:memory(usage,max)]),
    % logger:log(notice, "recon_alloc:cache_hit_rates() : ~p ~n", [recon_alloc:cache_hit_rates()]).
    % recon_alloc:sbcs_to_mbcs(max),
    % recon_alloc:average_block_sizes(max),
    % recon_alloc:memory(usage,current),
    % recon_alloc:memory(usage,max),
