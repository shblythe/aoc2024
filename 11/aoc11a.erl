-module(aoc11a).
-export([do/1]).
-compile(export_all).

blink(Stones) ->
    lists:flatten(lists:map(
        fun (Stone) ->
            Str = integer_to_list(Stone),
            Len = length(Str),
            if
                Stone == 0 -> 1;
                (Len rem 2) == 0 -> [
                    list_to_integer(string:substr(Str,1,Len div 2)),
                    list_to_integer(string:substr(Str,Len div 2+1))
                ];
                true -> Stone * 2024
            end
        end,
        Stones
    )).

blinks(0, Stones) -> Stones;
blinks(N, Stones) -> blinks(N-1, blink(Stones)).

do(File) ->
    code:add_path(".."),
    Stones=helpers:read_file_of_ints(File),
    Stones25 = blinks(25,Stones),
    io:format("Part 1: ~w~n",[length(Stones25)]).


% 40 failed
% 25 followed by 25 also fails, so there must be a better way.

