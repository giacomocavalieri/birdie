-module(birdie_ffi_erl).
-export([terminal_width/0, clear/0, cursor_up/1, clear_line/0]).

terminal_width() ->
    case io:columns(user) of
        {ok, Width} -> {ok, Width};
        {error, _} -> {error, nil}
    end.

clear() ->
    io:format("\ec"),
    io:format("\e[H\e[J").

cursor_up(X) ->
    io:format("\x1b[" ++ integer_to_list(X) ++ "A").

clear_line() ->
    io:format("\x1b[2K").
