% @doc ballgame top level supervisor.
% @end
-module(ballgame_sup).

-behavior(supervisor).

% API
-export([start_link/0]).

% Callbacks
-export([init/1]).

%--- API -----------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%--- Callbacks -----------------------------------------------------------------

init([]) ->
    Number = ballgame_util:get(number),
    SupFlags = #{strategy => one_for_one,
                 intensity => 3,
                 period => 10},
    ChildSpecs = [#{id => {player, Number},
                start => {player, start_link, [Number]},
                restart => permanent,
                type => worker,
                shutdown => 10000,
                modules => [player]}],
    {ok, {SupFlags, ChildSpecs}}.
