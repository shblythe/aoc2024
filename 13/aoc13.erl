-module(aoc13).
-export([do/1]).
% -compile(export_all).

process([], _) -> [];
process([L1, L2, L3 | Rest], PCorr) ->
    [_, _, Ax, Ay] = L1,
    [_, _, Bx, By] = L2,
    [_, WrongPx, WrongPy] = L3,
    Px = WrongPx + PCorr,
    Py = WrongPy + PCorr,
    LcmA = helpers:lcm(Ax, Ay),
    FactorX = LcmA div Ax,
    FactorY = LcmA div Ay,
    Bnum = Px * FactorX - Py * FactorY,
    Bden = Bx * FactorX - By * FactorY,
    % io:format("~w ~w~n",[Bnum, Bden]),
    Result = case Bnum rem Bden == 0 of
        false -> none;
        true ->
            B = Bnum div Bden,
            Anum = Px - Bx * B,
            Aden = Ax,
            case Anum rem Aden == 0 of
                false -> none;
                true -> {Anum div Aden, B}
            end
    end,
    % io:format("~w~n", [Result]),
    [Result] ++ process(Rest, PCorr).

do(File) ->
    code:add_path(".."),
    Contents = helpers:read_file_of_int_rows(File),
    Part1Combinations = lists:filter(fun (R) -> R /= none end, process(Contents, 0)),
    Part2Combinations = lists:filter(fun (R) -> R /= none end, process(Contents, 10000000000000)),
    Part1 = lists:foldl(
        fun ({A, B}, Sum) -> Sum + A * 3 + B end,
        0,
        Part1Combinations
    ),
    Part2 = lists:foldl(
        fun ({A, B}, Sum) -> Sum + A * 3 + B end,
        0,
        Part2Combinations
    ),
    io:format("Part1: ~w~nPart2: ~w~n", [Part1, Part2]).