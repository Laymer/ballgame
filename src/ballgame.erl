-module(ballgame).

-export([start/0,
         stop/0]).

%% @doc Start the application.
start() ->
  application:ensure_all_started(ballgame).

%% @doc Stop the application.
stop() ->
  application:stop(ballgame).
