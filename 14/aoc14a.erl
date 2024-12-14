-module(aoc14a).
-export([do/1]).
% -compile(export_all).

make_positive(Value, Modulo) ->
    case Value < 0 of
        false -> Value;
        true -> Value + Modulo
    end.

move_robots(Robots, SizeX, SizeY, Num) ->
    lists:map(
        fun([Px, Py, Vx, Vy]) -> [(Px + Vx * Num) rem SizeX, (Py + Vy * Num) rem SizeY, Vx, Vy] end,
        Robots
    ).

part1(RobotsPosV, SizeX, SizeY) ->
    Robots100 = move_robots(RobotsPosV, SizeX, SizeY, 100),
    Qtl = length(lists:filter(fun([Px, Py, _, _]) -> (Px < SizeX div 2) and (Py < SizeY div 2) end, Robots100)),
    Qbl = length(lists:filter(fun([Px, Py, _, _]) -> (Px < SizeX div 2) and (Py > SizeY div 2) end, Robots100)),
    Qtr = length(lists:filter(fun([Px, Py, _, _]) -> (Px > SizeX div 2) and (Py < SizeY div 2) end, Robots100)),
    Qbr = length(lists:filter(fun([Px, Py, _, _]) -> (Px > SizeX div 2) and (Py > SizeY div 2) end, Robots100)),
    Qtl * Qbl * Qtr * Qbr.

do(File) ->
    code:add_path(".."),
    Robots = helpers:read_file_of_int_rows(File),
    [SizeX, SizeY] = case File of
        "example" -> [11, 7];
        _ -> [101, 103]
    end,
    RobotsPosV = lists:map(
        fun([Px, Py, Vx, Vy]) -> [Px, Py, make_positive(Vx, SizeX), make_positive(Vy, SizeY)] end,
        Robots
    ),
    part1(RobotsPosV, SizeX, SizeY).