%%%-------------------------------------------------------------------
%% @doc ballgame constants definitions
%% @end
%%%-------------------------------------------------------------------

-include_lib("kernel/include/file.hrl").

%%====================================================================
%% Time Intervals (ms)
%%====================================================================

-define(MS,             20).
-define(ONE,          1000).
-define(THREE,        3000).
-define(FIVE,         5000).
-define(TEN,         10000).
-define(HMIN,        30000).
-define(MIN,         60000).

%%====================================================================
%% Timers
%%====================================================================

-define(TIME_MULTIPLIER,      lists:last(tuple_to_list(application:get_env(ballgame, time_multiplier, {ok, 1})))).
-define(SLEEP(Interval),                                     timer:sleep((round(Interval/?TIME_MULTIPLIER)))).

-define(PAUSEMS,                     ?SLEEP(?MS)).
-define(PAUSE1,                     ?SLEEP(?ONE)).
-define(PAUSE3,                   ?SLEEP(?THREE)).
-define(PAUSE5,                    ?SLEEP(?FIVE)).
-define(PAUSE10,                    ?SLEEP(?TEN)).
-define(PAUSEHMIN,                 ?SLEEP(?HMIN)).
-define(PAUSEMIN,                   ?SLEEP(?MIN)).

%%====================================================================
%% Conversions
%%====================================================================

-define(TOS(Ms),   Ms/?ONE).

%%====================================================================
%% Records
%%====================================================================

-record(rec, {
    tab = [],
    int = 0
}).

%%====================================================================
%% TEAMS
%%====================================================================


-define(ALL,     lists:seq(1,12,1) ).

-define(TEAM1,     lists:seq(1,4,1) ).
-define(TEAM2,      lists:seq(5,8,1) ).
-define(TEAM3,   lists:seq(9,12,1) ).

-define(PLAYER(Number),   list_to_atom(unicode:characters_to_list(["ballgame@my_grisp_board", "_", integer_to_list(Number)], utf8))).

-define(FAKEPLAYER(Name),   list_to_atom(unicode:characters_to_list(["ballgame@", Name], utf8))).

-define(TEAM(Numbers),   [ ?PLAYER(X) || X <- Numbers ] ).
-define(FAKETEAM(Names),   [ ?FAKEPLAYER(X) || X <- Names ] ).
