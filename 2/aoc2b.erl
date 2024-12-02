-module(aoc2b).
-export([do/1, is_safe_damper/2]).

is_safe_damper(First, [_]) -> aoc2a:is_safe(First);
is_safe_damper(First, [X,Y | SecondRest]) ->
    aoc2a:is_safe(First ++ [X] ++ SecondRest) or aoc2a:is_safe(First ++ [Y] ++ SecondRest)
    or is_safe_damper(First ++ [X], [Y] ++ SecondRest).
is_safe_damper(List) -> is_safe_damper([], List).

count_safe(List, Accumulator) ->
    %io:format("~w ~w~n",[List, is_safe_damper(List)]),
    Accumulator + case is_safe_damper(List) of
        true -> 1;
        false -> 0
    end.

do(File) ->
    code:add_path(".."),
    Reports = helpers:read_file_of_int_rows(File),
    %io:format("~w~n", [Reports]),
    lists:foldl(fun(X,A) -> count_safe(X,A) end, 0, Reports).