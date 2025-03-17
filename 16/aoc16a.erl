-module(aoc16a).
-export([do/1]).
-compile(export_all).

%%% Possible strategy:
%%% Break down the map into nodes at Start, End and junctions.
%%% If we are counting a turn as a costly move, then a junction can have
%%% only 2 exits, if they're at right angles.
%%% Then we can easily calculate the cost to travel between two adjacent nodes
%%% by travelling in each direction from the node.
%%% We can also ignore nodes that end in dead-ends.
%%% A node will look like {Coords, Dir} - e.g. {{2,3}, east}
%%% If its direction is 'finish' that means the direction of entry doesn't matter
%%% and we'll never exit.

%%% Or, less naively, could implement this as an a_star algorithm, where the possible neighbours
%%% of each location are a step forward, or a rotation through 90 degrees in either direction.
%%% Then h(n) would be the cost of the moves and rotations needed to get to the finish, assuming no walls.

left90(Dir) ->
    case Dir of
        north -> west;
        west -> south;
        south -> east;
        east -> north
    end.

right90(Dir) ->
    case Dir of
        north -> east;
        east -> south;
        south -> west;
        west -> north
    end.

find_node_positions([], _, Nodes) -> Nodes;
find_node_positions([{CellX, CellY} | RestMaze], Maze, Nodes) ->
    ExitWest = lists:member({CellX-1, CellY}, Maze),
    ExitEast = lists:member({CellX+1, CellY}, Maze),
    ExitNorth = lists:member({CellX, CellY-1}, Maze),
    ExitSouth = lists:member({CellX, CellY+1}, Maze),
    case (ExitWest or ExitEast) and (ExitNorth or ExitSouth) of
        false -> find_node_positions(RestMaze, Maze, Nodes);
        true ->
            NewNodes = lists:flatten([
                case ExitWest of true -> {{CellX, CellY}, west}; false -> [] end,
                case ExitEast of true -> {{CellX, CellY}, east}; false -> [] end,
                case ExitNorth of true -> {{CellX, CellY}, north}; false -> [] end,
                case ExitSouth of true -> {{CellX, CellY}, south}; false -> [] end
            ]),
            find_node_positions(RestMaze, Maze, NewNodes ++ Nodes)
    end.

%%% Now we search for the shortest path from StartNode to EndNode.
%%% Start from StartNode, identify the paths which are there, and traverse each
%%% of those paths until we come to another node.
%%% Is all of this pointless?  Do we just need a recursive function that builds a list
%%% of all possible paths?

move_transform({{X, Y}, Dir}) ->
    case Dir of
        west -> {{X-1, Y}, Dir};
        east -> {{X+1, Y}, Dir};
        north -> {{X, Y-1}, Dir};
        south -> {{X, Y+1}, Dir}
    end.

%%% Let's just try that...
%%%
%%% find_path returns a list containing the cost of a route to finish, or an empty list
%%% if no route.
%%%
%%% If this takes too long, maybe I can cache results somehow?
%%% Or should I do dead-end filling first?
find_paths({Coords, Dir}, Maze, Cost, EndPos, PathSoFar) ->
    % io:format("~w ~w ~w ~w~n", [Coords, Dir, Cost, EndPos]),
    case lists:member(Coords, Maze) and not lists:member(Coords, PathSoFar) of
        false -> [];
        true -> case Coords == EndPos of
            true -> io:format("Done ~w~n", [Cost]), [Cost];
            false ->
                find_paths(move_transform({Coords, Dir}), Maze, Cost+1, EndPos, [Coords] ++ PathSoFar) ++
                find_paths(move_transform({Coords, left90(Dir)}), Maze, Cost+1001, EndPos, [Coords] ++ PathSoFar) ++
                find_paths(move_transform({Coords, right90(Dir)}), Maze, Cost+1001, EndPos, [Coords] ++ PathSoFar)
        end
    end.

display_map(MaxX, MaxY, StartPos, EndPos, Maze) ->
    lists:foreach(
        fun (Y) ->
            lists:foreach(
                fun (X) ->
                    InMaze = lists:member({X, Y}, Maze),
                    if
                        {X, Y} == StartPos -> io:format("S");
                        {X, Y} == EndPos -> io:format("E");
                        InMaze -> io:format(".");
                        true -> io:format("#")
                    end
                end,
                lists:seq(0, MaxX)
            ),
            io:format("~n")
        end,
        lists:seq(0, MaxY)
    ).

exits({X, Y}, Maze) ->
    PossExits = [{X-1, Y}, {X+1, Y}, {X, Y-1}, {X, Y+1}],
    ListWithoutExits = lists:subtract(Maze, PossExits),
    lists:subtract(Maze, ListWithoutExits).

is_dead_end(Coord, StartPos, EndPos, Maze) ->
    lists:member(Coord, Maze) andalso
    Coord /= StartPos andalso
    Coord /= EndPos andalso
    length(exits(Coord, Maze)) < 2.

check_dead_end({X, Y}, StartPos, EndPos, Maze) ->
    case is_dead_end({X, Y}, StartPos, EndPos, Maze) of
        true -> [{X, Y}];
        false -> []
    end.

find_dead_ends_row(Y, MaxX, StartPos, EndPos, Maze) ->
    lists:foldl(
        fun (X, List) -> [ check_dead_end({X, Y}, StartPos, EndPos, Maze) | List] end,
        [],
        lists:seq(0, MaxX)
    ).

find_dead_ends(MaxX, MaxY, StartPos, EndPos, Maze) ->
    lists:flatten(
        lists:foldl(
            fun (Row, List) -> [find_dead_ends_row(Row, MaxX, StartPos, EndPos, Maze) | List] end,
            [],
            lists:seq(0,MaxY)
        )
    ).

fill_dead_end(Coord, StartPos, EndPos, Maze) ->
    case is_dead_end(Coord, StartPos, EndPos, Maze) of
        false -> Maze;
        true ->
            [NextCoord] = exits(Coord, Maze),
            fill_dead_end(NextCoord, StartPos, EndPos, lists:delete(Coord, Maze))
    end.

fill_dead_ends([], _, _, Maze) -> Maze;
fill_dead_ends([DeadEnd | Rest], StartPos, EndPos, Maze) ->
    fill_dead_ends(Rest, StartPos, EndPos, fill_dead_end(DeadEnd, StartPos, EndPos, Maze)).

do(File) ->
    code:add_path(".."),
    Input = helpers:read_file_of_string_list(File),
    {MaxX, MaxY} = helpers:string_list_extents(Input),
    [StartPos] = helpers:find_coordinates_in_string_list(Input, ["S"], 0),
    [EndPos] = helpers:find_coordinates_in_string_list(Input, ["E"], 0),
    Maze = [StartPos, EndPos] ++ helpers:find_coordinates_in_string_list(Input, ["\\."], 0),
    StartNode = {StartPos, east},
    % display_map(MaxX, MaxY, StartPos, EndPos, Maze),
    DeadEnds = find_dead_ends(MaxX, MaxY, StartPos, EndPos, Maze),
    MazeNoDeadEnds = fill_dead_ends(DeadEnds, StartPos, EndPos, Maze),
    io:format("Eliminated ~w dead ends~n", [length(DeadEnds)]),
    display_map(MaxX, MaxY, StartPos, EndPos, MazeNoDeadEnds),
    % EndNode = {EndPos, finish},
    % Nodes = [StartNode] ++ find_node_positions(Maze, Maze, []),
    lists:min(find_paths(StartNode, Maze, 0, EndPos, [])).