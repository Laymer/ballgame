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
    % {[Numbers],Team} = case ballgame_util:get(testing) of
    %     true ->
    %         ?FAKETEAM(ballgame_util:get(fakeplayers));
    %     false ->
    %         ?TEAM(ballgame_util:get(players));
    %
    % end
    Players = ballgame_util:get(players),
    Team = team(Players),
    % _L = [ ballgame_util:join(X) ||
    L = [ ballgame_util:join(X) ||
        X <- Team,
        X =/= node(),
        net_adm:ping(X) =:= pong ],
    % M = ballgame_util:members(),
    % logger:log(info, "Joined = ~p ~n", [M]),
    logger:log(info, "Joined = ~p ~n", [L]),
    L.

team(Players) ->
    [ name(X) || X <- Players ].

name(Host) when is_integer(Host) ->
    ?PLAYER(Host);
name(Host) when is_list(Host) ->
    ?FAKEPLAYER(Host).
    % list_to_atom(unicode:characters_to_list(["ballgame@", Host], utf8)).
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
