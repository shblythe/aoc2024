-module(aoc11b).
-export([do/1, cache_hits/0]).
% -compile(export_all).

%%% Blinks a single stone N times and returns the total number of stones
blink(0, _) -> 1;
blink(N, Stone) ->
    Cached = get({N, Stone}),
    if
        Cached /= undefined ->
            % io:format("Cache hit: ~w ~w~n", [N, Stone]),
            {Value, Hits} = Cached,
            put({N, Stone}, {Value, Hits+1}),
            Value;
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
            put({N, Stone}, {Result, 0}),
            Result
    end.

blinks(N, Stones) ->
    lists:foldl(fun (Stone, Sum) -> Sum + blink(N, Stone) end, 0, Stones).

cache_hits() ->
    lists:sort(fun ({_, Hits1}, {_, Hits2}) -> Hits1 > Hits2 end,
    lists:map(
        fun (Key) ->
            {_, Hits} = get(Key),
            {Key, Hits}
        end,
        get_keys()
    )).

do(File) ->
    code:add_path(".."),
    erase(),    % clear the cache
    Stones=helpers:read_file_of_ints(File),
    blinks(75, Stones).

