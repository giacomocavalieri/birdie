-module(birdie_lsp_ffi).

-export([start/0, read_line/1, read_bytes/2]).

start() ->
    Port = open_port({fd, 0, 1}, [binary, stream, eof]),
    {Port, <<>>}.

read_line({Port, Acc}=State) ->
    case binary:split(Acc, <<"\r\n"/utf8>>) of
        [Line, Rest] ->
            NewState = {Port, Rest},
            {ok, {NewState, Line}};
        _ ->
            case get_more(State) of
                {ok, NewState} -> read_line(NewState);
                {error, Reason} -> {error, Reason}
            end
    end.

read_bytes({Port, Acc}=State, N) ->
    case Acc of
        % We have already read enough bytes to return something!
        <<Result:N/binary, Rest/binary>> ->
            NewState = {Port, Rest},
            {ok, {NewState, Result}};

        % Otherwise we need to wait and receive something from the
        % port until we have enough bytes.
        _ ->
            case get_more(State) of
                {ok, NewState} -> read_bytes(NewState, N);
                {error, Reason} -> {error, Reason}
            end
    end.

get_more({Port, Acc}) ->
    receive
        {Port, {data, Bytes}} ->
            {ok, {Port, <<Acc/binary, Bytes/binary>>}};
        Reason -> {error, Reason}
    end.
