-module(birdie_ffi).
-export([get_line/1, is_windows/0]).

get_line(Prompt) ->
    case io:get_line(Prompt) of
        eof -> {error, nil};
        {error, _} -> {error, nil};
        Data when is_binary(Data) -> {ok, Data};
        Data when is_list(Data) -> {ok, unicode:characters_to_binary(Data)}
    end.

is_windows() ->
    case os:type() of
        {win32, _} -> true;
        _ -> false
    end.
