-module(aoc6).
-export([do/1]).
% -compile(export_all).

add_tuple2({A1, B1}, {A2, B2}) ->
    {A1+A2, B1+B2}.

turn_90({X,Y}) -> {-Y,X}.

%%% Take one step on the walk
%%% Pos is the current position as a tuple {x,y}
%%% Dir is the direction as a tuple:
%%%     {0,-1} : up
%%%     {0,1}: down
%%%     {1,0} : right
%%%     {-1,0}: left
%%% MaxX and MaxY give the extents of the map
%%% Obstacles is the list of obstacles
%%% Trail is the route walked so far, as a list of tuples: {Pos, Dir}
%%% Returns {Inf, Trail} where Inf is true if guard is in an infinite loop
%%% or false if she walked off the map, Trail is the final trail walked
walk(Pos, Dir, MaxX, MaxY, Obstacles, Trail) ->
    RawNewPos = add_tuple2(Pos, Dir),
    NewDir = case lists:member(RawNewPos, Obstacles) of
            true -> turn_90(Dir);
            false -> Dir
        end,
    NewPos = add_tuple2(Pos, NewDir),
    case lists:member({NewPos, NewDir}, Trail) of
        true -> {true, [{Pos, Dir}] ++ Trail};
        false ->
            case helpers:outside_map(NewPos, MaxX, MaxY) of
                    true -> {false, [{Pos, Dir}] ++ Trail};
                    false -> walk(NewPos, NewDir, MaxX, MaxY, Obstacles, [{Pos, Dir}] ++ Trail)
                 end
    end.

do(File) ->
    code:add_path(".."),
    Map = helpers:read_file_of_string_list(File),
    Obstacles = helpers:find_coordinates_in_string_list(Map, ["#"], 0),
    [Guard | _] = helpers:find_coordinates_in_string_list(Map, ["\\^"], 0),
    {MaxX, MaxY} = helpers:string_list_extents(Map),
    {_, Trail} = walk(Guard, {0,-1}, MaxX, MaxY, Obstacles, []),
    TrailPositions = lists:uniq(lists:map(fun ({Pos, _}) -> Pos end, Trail)),
    io:format("Part 1: ~w~n",[length(TrailPositions)]),
    % AllPositions = lists:foldl(
    %         fun(X, Coords) ->
    %             lists:foldl(fun(Y, XYs) -> [{X,Y}] ++ XYs end, [], lists:seq(0, MaxY)) ++ Coords
    %         end,
    %         [],
    %         lists:seq(0, MaxX)
    %     ),
    PossibleObstaclePositions = TrailPositions, % only need to try places Guard would hit!
    CountSuccessfulPositions = lists:foldl(
        fun(NewObstacle, InfiniteCount) ->
            io:format("~w~n", [NewObstacle]),
            {Infinite, _} = walk(Guard, {0,-1}, MaxX, MaxY, Obstacles ++ [NewObstacle], []),
            case Infinite of
                true -> 1 + InfiniteCount;
                false -> InfiniteCount
            end
        end,
        0,
        PossibleObstaclePositions
    ),
    io:format("Part 2: ~w~n",[CountSuccessfulPositions]).
