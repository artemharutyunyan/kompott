%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module containing common functons
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(ttcommon).

-export([format_cowboy_reply/1]).
-export([utc_timestamp/0]).

%% Formats result of a given function call for passing to cowboy_req:reply
format_cowboy_reply(F) ->
   try F() of
      %% Return 200 if the call returned 'ok'
      {ok, json, Json} ->
         {200, convert(json, binary, Json)};
         %% Return 500 if the call returned 'error' (or something unexpected)
      {error, json, Json} ->
         {500, convert(json, binary, Json)};
      E ->
         {500, format_string("~8192p", [E])}
   catch
      T:E ->
      Stack = erlang:get_stacktrace(),
      lager:error("Exception ~p:~p~nStack: ~p", [T, E, Stack]),
      {500, format_string("Exception ~p:~p~nStack: ~8192p", [T, E, Stack])}
   end.

%% Internal function for performing conversions between various types
convert(json, binary, J) ->
   iolist_to_binary(jsx:encode(J)).

%% Internal function for formatting binary strings
format_string(Format, String) ->
   list_to_binary(io_lib:format(Format, String)).

%% UTC timestamp
utc_timestamp() ->
   EpochSeconds = 719528*24*3600,
   NowSeconds = calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(now())),
   SecondsSinceEpoch = NowSeconds - EpochSeconds,
   SecondsSinceEpoch.

