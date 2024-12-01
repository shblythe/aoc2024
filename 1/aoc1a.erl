-module(aoc1a).
-export([do/1]).

get_total_column_differences([], [], Accum) -> Accum;
get_total_column_differences(Left, Right, Accum) ->
    %io:format("get_total_column_differences(~w, ~w, ~w)~n", [Left, Right, Accum]),
    [LeftValue | LeftRest] = Left,
    [RightValue | RightRest] = Right,
    NewAccum = Accum + abs(LeftValue - RightValue),
    %io:format("~w = ~w + abs(~w - ~w)~n",[NewAccum, Accum, LeftValue, RightValue]),
    get_total_column_differences(LeftRest, RightRest, NewAccum).

get_total_column_differences(Left, Right) -> get_total_column_differences(Left, Right, 0).

count_occurrences(_, [], Accum) -> Accum;
count_occurrences(Value, [Value | Rest], Accum) -> count_occurrences(Value, Rest, Accum + 1);
count_occurrences(Value, [_ | Rest], Accum) -> count_occurrences(Value, Rest, Accum).

count_occurrences(Value, List) -> count_occurrences(Value, List, 0).

calculate_similarity([], _, Accum) -> Accum;
calculate_similarity(Left, Right, Accum) ->
    [LeftValue | LeftRest] = Left,
    calculate_similarity(LeftRest, Right, Accum + LeftValue * count_occurrences(LeftValue, Right)).

calculate_similarity(Left, Right) -> calculate_similarity(Left, Right, 0).

do(File) ->
    code:add_path(".."),
    io:format("Processing file \"~s\"~n", [File]),
    Tokens = helpers:read_file_of_ints(File),
    [Left, Right] = helpers:split_2_columns(Tokens),
    SortedLeft = lists:sort(Left),
    SortedRight = lists:sort(Right),
    %io:format("Left  : ~w~n", [SortedLeft]),
    %io:format("Right : ~w~n", [SortedRight]),
    Result = get_total_column_differences(SortedLeft, SortedRight),
    io:format("Result A: ~w~n", [Result]),
    io:format("Result B: ~w~n", [calculate_similarity(Left, Right)])
.
