% @doc ballgame top level supervisor.
% @end
-module(ballgame_sup).

-behavior(supervisor).

% API
-export([start_link/0]).

% Callbacks
-export([init/1]).

%--- API -----------------------------------------------------------------------


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
    [{stress_test,ballgame_util:get(first_hand)}]).
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
init([Args]) ->
    % Number = ballgame_util:get(number),
    SupFlags = #{strategy => one_for_one,
                 intensity => 3,
                 period => 10},
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
    ChildSpecs = [#{id => ballgame_matchmaker,
                            start => {ballgame_matchmaker, start_link, []},
                            restart => permanent,
                            type => worker,
                            shutdown => 10000,
                            modules => [ballgame_matchmaker]}],
    {ok, {SupFlags, ChildSpecs}}.
