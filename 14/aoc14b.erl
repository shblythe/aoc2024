-module(aoc14b).
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

display_robots(Robots, SizeX, SizeY) ->
    RobotsPosMap = maps:from_list(lists:map(fun ([Px, Py, _, _]) -> {{Px, Py}, 1} end, Robots)),
    lists:foreach(
        fun (Y) ->
            lists:foreach(
                fun (X) ->
                    case maps:get({X, Y}, RobotsPosMap, 0) of
                        1 -> io:format("*");
                        0 -> io:format(" ")
                    end
                end,
                lists:seq(0, SizeX - 1)
            ),
            io:format("~n")
        end,
        lists:seq(0, SizeY - 1)
    ).

clear_screen() ->
    io:format("\e[H\e[J").

find_tree(Robots, Count, SizeX, SizeY) ->
    case Count == SizeX*SizeY of
        true -> false;
        false ->
            % Clustering of points, just based on average distance from the first point in the list
            [[P1x, P1y, _, _] | Rest] = Robots,
            TotalDistance = lists:foldl(
                fun([Pnx, Pny, _, _], Sum) -> Sum + math:sqrt(math:pow(Pnx-P1x,2)+math:pow(Pny-P1y,2)) end,
                0,
                Rest),
            MeanDistance = TotalDistance / length(Rest),
            _ = if MeanDistance < 25 ->
                    timer:sleep(1000),
                    clear_screen(),
                    io:format("~w~n", [Count]),
                    display_robots(Robots, SizeX, SizeY),
                    io:format("~w~n", [MeanDistance]),
                    progress_bar(Count, SizeX*SizeY, SizeX);
                    % io:read("Next...");
            true -> 0
            end,
            Moved = move_robots(Robots, SizeX, SizeY, 1),
            find_tree(Moved, Count+1, SizeX, SizeY)
    end.

progress_bar(Value, Max, MaxChars) ->
    io:format("~s~n", [lists:duplicate((Value * MaxChars) div Max, $*)]).

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
    find_tree(RobotsPosV, 0, SizeX, SizeY).