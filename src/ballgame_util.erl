-module(ballgame_util).

-include("ballgame.hrl").

-compile({nowarn_export_all}).

-compile(export_all).

%%====================================================================
%% Utility functions
%%====================================================================

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

%%====================================================================
%% Clustering functions
%%====================================================================


%%====================================================================
%% NO LOGGING
%%====================================================================
-ifdef(SILENT).

seek_neighbors() ->
  net_adm:ping_list(lists:filtermap(fun
    (Tup) ->
      case Tup of
        {host,_Addr,[Hostname]} ->
          {true, list_to_atom("ballgame@" ++ Hostname)};
        _ ->
          false
      end
    end, inet_db:get_rc())).

join(Host) ->
Manager = rpc:call(Host, partisan_peer_service, manager, []),
case Manager of
  partisan_hyparview_peer_service_manager ->
    Node = rpc:call(Host, Manager, myself, []),
    ok = partisan_peer_service:join(Node),
    Node;
  {error, Reason} ->
    {error, Reason}
end.

clusterize() ->
    [ ballgame_util:join(X) ||
      X <- seek_neighbors(),
      X =/= node() ].

-else.
%%====================================================================
%% LOGGING
%%====================================================================

seek_neighbors() ->
  logger:log(info, "Pinging possible neighbors ~n"),

  Rc = inet_db:get_rc(),
  Hosts = lists:filtermap(fun
    (Tup) ->
      case Tup of
        {host,_Addr,[Hostname]} ->
          % [Sname|_] = string:split(atom_to_binary(node(),utf8),"@"),
          % Str = unicode:characters_to_list([Sname,"@",Hostname],utf8),
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

team(Players) ->
  [ name(X) || X <- Players ].

name(Host) when is_integer(Host) ->
  ?PLAYER(Host);
name(Host) when is_atom(Host) ->
  list_to_atom(unicode:characters_to_list(["ballgame@", atom_to_list(Host)], utf8));
name(Host) when is_list(Host) ->
  [list_to_atom(unicode:characters_to_list(["ballgame@", atom_to_list(X)], utf8)) || X <- lists:flatten(Host)].
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
