% @doc ballgame top level supervisor.
% @end
-module(ballgame_sup).

-behavior(supervisor).

% API
-export([start_link/0]).
-export([start_link/1]).
-export([start_temp/0]).
-export([start_player/0]).

% Callbacks
-export([init/1]).

%--- API -----------------------------------------------------------------------

-define(SUPFLAGS,     #{strategy => one_for_one,
                        intensity => 3,
                        period => 10}).

-define(TEMP_CHILD(Args),     #{id => ballgame_temp,
                                start => {ballgame_temp, start_link, Args},
                                restart => permanent,
                                type => worker,
                                shutdown => 10000,
                                modules => [ballgame_temp]}).

-define(PLAYER_CHILD(Args),     #{id => {player, Args},
                                start => {player, start_link, Args},
                                restart => permanent,
                                type => worker,
                                shutdown => 10000,
                                modules => [player]}).

-define(BALLGAME_MATCHMAKER_CHILD(),     #{id => ballgame_matchmaker,
                                start => {ballgame_matchmaker, start_link, []},
                                restart => permanent,
                                type => worker,
                                shutdown => 10000,
                                modules => [ballgame_matchmaker]}).
% -ifdef (SHELL).
%     start_link() ->
%         supervisor:start_link({local, ?MODULE},
%         ?MODULE,
%         [{stress_test,ballgame_util:get(first_hand)}]).
% -else.
%     start_link() ->
%         supervisor:start_link({local, ?MODULE},
%         ?MODULE,
%         [ballgame_util:get(number)]).
% -endif.
% -ifndef (SILENT).
start_link() ->
    supervisor:start_link({local, ?MODULE},
    ?MODULE,
    [{stress_test, ballgame_util:get(first_hand)}]).

start_link(awset) ->
    supervisor:start_link({local, ?MODULE},
    ?MODULE,
    [ {awset, ballgame_util:get(packet_config)} ]).


start_temp() ->
  NavConfig = ballgame_util:get(nav_config),
  supervisor:start_child(?MODULE, ?TEMP_CHILD([NavConfig])).

start_player() ->
  PacketConfig = ballgame_util:get(packet_config),
  supervisor:start_child(?MODULE, ?PLAYER_CHILD([{awset, PacketConfig}])).
% -else.
%     start_link() ->
%         supervisor:start_link({local, ?MODULE},
%         ?MODULE,
%         [ballgame_util:get(number)]).
% -endif.

%--- Callbacks -----------------------------------------------------------------
% ballgame_matchmaker:spawn_players().
% sys:get_status(player1).
% process_info(whereis(player1)).
% sys:statistics(whereis(player1),true),sys:statistics(whereis(player2),true).
% sys:statistics(whereis(player1),get).
% sys:statistics(whereis(player2),get).net_adm:ping(ballgame@my_grisp_board_1).
% gen_server:cast(whereis(player1), {stress, ballgame@my_grisp_board_1}).
% gen_server:cast(whereis(player2), {stress, ballgame@my_grisp_board_1}).
% gen_server:cast()
% init([{awset, PacketConf }]) ->
%     SupFlags = #{strategy => one_for_one,
%     intensity => 3,
%     period => 10},
%     ChildSpecs = [#{id => ballgame_matchmaker,
%                     start => {ballgame_matchmaker, start_link, []},
%                     restart => permanent,
%                     type => worker,
%                     shutdown => 10000,
%                     modules => [ballgame_matchmaker]},
%                 #{id => player,
%                         start => {player, start_link, [{awset, PacketConf}]},
%                         restart => permanent,
%                         type => worker,
%                         shutdown => 10000,
%                         modules => [player]}],
%     {ok, {SupFlags, ChildSpecs}}.

init([Args]) ->
    {ok, {?SUPFLAGS, [
                        ?BALLGAME_MATCHMAKER_CHILD()
                      ]}}.
    % Number = ballgame_util:get(number),
    % SupFlags = #{strategy => one_for_one,
    %              intensity => 3,
    %              period => 10},
    % ChildSpecs = [],
    % {ok, {SupFlags, ChildSpecs}}.



% ChildSpecs = [#{id => {player, Args},
%                         start => {player, start_link, [Args]},
%                         restart => permanent,
%                         type => worker,
%                         shutdown => 10000,
%                         modules => [player]},
%             #{id => ballgame_matchmaker,
%                         start => {ballgame_matchmaker, start_link, []},
%                         restart => permanent,
%                         type => worker,
%                         shutdown => 10000,
%                         modules => [ballgame_matchmaker]}],
