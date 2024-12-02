-module(aoc2a).
-export([do/1, is_safe/1]).

pair_safe_range(X,Y) -> (abs(X-Y) =< 3) and (abs(X-Y) >= 1).

is_safe([], _) -> true;
is_safe([_], _) -> true;
is_safe([X,Y | Rest], increasing) -> (Y>X) and pair_safe_range(X,Y) and is_safe([Y] ++ Rest, increasing);
is_safe([X,Y | Rest], decreasing) -> (Y<X) and pair_safe_range(X,Y) and is_safe([Y] ++ Rest, decreasing);
is_safe([X,Y | Rest], either) ->
    pair_safe_range(X,Y) and is_safe([Y] ++ Rest,
                                     case (Y>X) of
                                        true -> increasing;
                                        false -> decreasing
                                    end).
is_safe(List) -> is_safe(List, either).

count_safe(List, Accumulator) ->
    %io:format("~w ~w~n",[List, is_safe(List)]),
    Accumulator + case is_safe(List) of
        true -> 1;
        false -> 0
    end.

do(File) ->
    code:add_path(".."),
    Reports = helpers:read_file_of_int_rows(File),
    %io:format("~w~n", [Reports]),
    lists:foldl(fun(X,A) -> count_safe(X,A) end, 0, Reports).