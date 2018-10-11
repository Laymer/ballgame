% @doc ballgame public API.
% @end
-module(ballgame).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

start(_Type, _Args) -> ballgame_sup:start_link().

stop(_State) -> ok.
