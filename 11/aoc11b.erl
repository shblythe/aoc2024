-module(aoc11b).
-export([do/1]).
-compile(export_all).

%%% Blinks a single stone N times and returns the total number of stones
blink(0, _) -> 1;
blink(N, Stone) ->
    if
        Stone == 0 -> blink(N-1, 1);
        true ->
            Str = integer_to_list(Stone),
            Len = length(Str),
            if
                (Len rem 2) == 0 ->
                    blink(N-1, list_to_integer(string:substr(Str,1,Len div 2))) +
                    blink(N-1, list_to_integer(string:substr(Str,Len div 2+1)));
                true ->
                    blink(N-1, Stone * 2024)
            end
    end.

do(File) ->
    code:add_path(".."),
    Stones=helpers:read_file_of_ints(File).

