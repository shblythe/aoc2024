-module(aoc11b).
-export([do/1]).
-compile(export_all).

%%% Blinks a single stone N times and returns the total number of stones
blink(0, _) -> 1;
blink(N, Stone) ->
    Cached = get({N, Stone}),
    if
        Cached /= undefined ->
            io:format("Cache hit: ~w ~w~n", [N, Stone]),
            Cached;
        true ->
            Result = if
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
            end,
            put({N, Stone}, Result),
            Result
    end.

blinks(N, Stones) ->
    lists:foldl(fun (Stone, Sum) -> Sum + blink(N, Stone) end, 0, Stones).

do(File) ->
    code:add_path(".."),
    erase(),    % clear the cache
    Stones=helpers:read_file_of_ints(File),
    blinks(75, Stones).

