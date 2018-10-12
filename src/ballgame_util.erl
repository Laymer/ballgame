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
