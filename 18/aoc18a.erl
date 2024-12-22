-module(aoc18a).
% -export([do/1]).
-compile(export_all).

reconstruct_path(CameFrom, Current, TotalPath) ->
    case maps:is_key(Current, CameFrom) of
        false -> TotalPath;
        true -> reconstruct_path(CameFrom, maps:get(Current, CameFrom), [Current | TotalPath])
    end.

a_star_process_neighbour([], _Current, _FunH, OpenSet, CameFrom, GScore, FScore, _Goal) -> {OpenSet, CameFrom, GScore, FScore};
a_star_process_neighbour([Neighbour | Rest], Current, FunH, OpenSet, CameFrom, GScore, FScore, Goal) ->
    % io:format("a_star_process_neighbour ~w~n", [[Neighbour | Rest]]),
    TentativeGScore = maps:get(Current, GScore, 9999999999) + 1,
    case TentativeGScore < maps:get(Neighbour, GScore, 9999999999) of
        false -> a_star_process_neighbour(Rest, Current, FunH, OpenSet, CameFrom, GScore, FScore, Goal);
        true ->
            NewCameFrom = maps:put(Neighbour, Current, CameFrom),
            NewGScore = maps:put(Neighbour, TentativeGScore, GScore),
            NewFScore = maps:put(Neighbour, TentativeGScore + FunH(Neighbour, Goal), FScore),
            NewOpenSet = sets:add_element(Neighbour, OpenSet),
            a_star_process_neighbour(Rest, Current, FunH, NewOpenSet, NewCameFrom, NewGScore, NewFScore, Goal)
    end.

a_star_process_neighbours(Current, FunH, Obstacles, OpenSet, CameFrom, GScore, FScore, Goal) ->
    {GoalX, GoalY} = Goal,
    {CurrentX, CurrentY} = Current,
    Neighbours = lists:flatten([
        case (CurrentX == GoalX) or lists:member({CurrentX + 1, CurrentY},Obstacles) of
            true -> [];
            false -> {CurrentX + 1, CurrentY}
        end,
        case (CurrentX == 0) or lists:member({CurrentX - 1, CurrentY},Obstacles) of
            true -> [];
            false -> {CurrentX - 1, CurrentY}
        end,
        case (CurrentY == GoalY) or lists:member({CurrentX, CurrentY + 1},Obstacles) of
            true -> [];
            false -> {CurrentX, CurrentY + 1}
        end,
        case (CurrentY == 0) or lists:member({CurrentX, CurrentY - 1},Obstacles) of
            true -> [];
            false -> {CurrentX, CurrentY - 1}
        end
    ]),
    a_star_process_neighbour(Neighbours, Current, FunH, OpenSet, CameFrom, GScore, FScore, Goal).

a_star(Start, Goal, FunH, Obstacles) ->
    OpenSet = sets:add_element(Start, sets:new()),
    CameFrom = #{},
    GScore = #{ Start => 0 },

    FScore = #{ Start => FunH(Start, Goal) },
    a_star_iter(Goal, FunH, Obstacles, OpenSet, CameFrom, GScore, FScore).

find_cheapest_in_openset(OpenSet, FScore) ->
    % io:format("~w ~w~n", [OpenSet, sets:to_list(OpenSet)]),
    OpenSetFScores = lists:map(fun (Node) -> {Node, maps:get(Node,FScore)} end, sets:to_list(OpenSet)),
    [{CheapestNode, _} | _] = lists:sort(fun ({_, S1}, {_, S2}) -> S1 < S2 end, OpenSetFScores),
    CheapestNode.

a_star_iter(Goal, FunH, Obstacles, OpenSet, CameFrom, GScore, FScore) ->

    case sets:size(OpenSet) of
        0 -> failure;
        _ ->
            Current = find_cheapest_in_openset(OpenSet, FScore),
            case Current == Goal of
                true -> reconstruct_path(CameFrom, Current, []);
                false ->
                    PurgedOpenSet = sets:del_element(Current, OpenSet),
                    {NewOpenSet, NewCameFrom, NewGScore, NewFScore} = a_star_process_neighbours(Current, FunH, Obstacles, PurgedOpenSet, CameFrom, GScore, FScore, Goal),
                    a_star_iter(Goal, FunH, Obstacles, NewOpenSet, NewCameFrom, NewGScore, NewFScore)
            end
    end.

taxicab({X1, Y1}, {X2, Y2}) -> X2 - X1 + Y2 - Y1.

do_a(File, NumBytes, Goal) ->
    code:add_path(".."),
    Input=helpers:read_file_of_int_rows(File),
    {Obstacles, _}=lists:split(NumBytes, lists:map(fun([A,B])->{A,B} end, Input)),
    % io:format("~w~n", [Obstacles]),
    length(a_star({0,0}, Goal, fun taxicab/2, Obstacles)).

do_a_example() -> do_a("example", 12, {6,6}).

do_a_real() -> do_a("18.dat", 1024, {70,70}).

find_obstacle_limit(Start, Goal, FunH, Obstacles, [NewObstacle | RestObstacles], Count) ->
    % io:format("find_obstacle_limit ~w~n", [Count]),
    NewObstacles = [NewObstacle | Obstacles],
    case a_star(Start, Goal, FunH, NewObstacles) of
        failure -> NewObstacle;
        _ -> find_obstacle_limit(Start, Goal, FunH, [NewObstacle | Obstacles], RestObstacles, Count+1)
    end.

do_b(File, NumBytes, Goal) ->
    code:add_path(".."),
    Input=helpers:read_file_of_int_rows(File),
    {StartingObstacles, ObstaclesToAdd}=lists:split(NumBytes, lists:map(fun([A,B])->{A,B} end, Input)),
    find_obstacle_limit({0,0}, Goal, fun taxicab/2, StartingObstacles, ObstaclesToAdd, NumBytes).

do_b_example() -> do_b("example", 12, {6,6}).

do_b_real() -> do_b("18.dat", 1024, {70,70}).