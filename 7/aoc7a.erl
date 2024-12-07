-module(aoc7a).
-export([do/1]).
% -compile(export_all).

can_be_true(Answer, [X]) ->
    Answer == X;

can_be_true(Answer, [X | Rest]) ->
    can_be_true(Answer - X, Rest) or
    ((Answer rem X == 0) and can_be_true(Answer div X, Rest)).

can_be_true([Answer | Terms]) -> can_be_true(Answer, lists:reverse(Terms)).

do(File) ->
    code:add_path(".."),
    Equations = helpers:read_file_of_int_rows(File),
    Result = lists:foldl(
        fun (Equation, Sum) -> Sum + case can_be_true(Equation) of
                true -> hd(Equation);
                false -> 0
            end
        end,
        0,
        Equations
    ),
    io:format("Part 1: ~w~n", [Result]).