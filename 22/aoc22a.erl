-module(aoc22a).
-compile(export_all).

next(Num) ->
    Num1 = ((Num * 64) bxor Num) rem 16777216,
    Num2 = ((Num1 div 32) bxor Num1) rem 16777216,
    ((Num2 * 2048) bxor Num2) rem 16777216.

next(Num, 0) -> Num;
next(Num, N) ->
    Next = next(Num),
    % io:format("~w~n", [Next]),
    next(Next, N-1).

do(File) ->
    code:add_path(".."),
    Numbers = helpers:read_file_of_ints(File),
    lists:foldl(fun (Num, Sum) -> Sum + next(Num, 2000) end, 0, Numbers).