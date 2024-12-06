-module(aoc6a).
-export([do/1]).
% -compile(export_all).

add_tuple2({A1, B1}, {A2, B2}) ->
    {A1+A2, B1+B2}.

turn_90({X,Y}) -> {-Y,X}.

outside_bounds(X, Min, Max) ->
    (X < Min) or (X > Max).

outside_map(Pos, MaxX, MaxY) ->
    {X, Y} = Pos,
    outside_bounds(X, 0, MaxX) or outside_bounds(Y, 0, MaxY).

%%% Take one step on the walk
%%% Pos is the current position as a tuple {x,y}
%%% Dir is the direction as a tuple:
%%%     {0,-1} : up
%%%     {0,1}: down
%%%     {1,0} : right
%%%     {-1,0}: left
%%% MaxX and MaxY give the extents of the map
%%% Obstacles is the list of obstacles
%%% Trail is the route walked so far, as a list of tuples
%%% Returns the final trail walked
walk(Pos, Dir, MaxX, MaxY, Obstacles, Trail) ->
    RawNewPos = add_tuple2(Pos, Dir),
    NewDir = case lists:member(RawNewPos, Obstacles) of
            true -> turn_90(Dir);
            false -> Dir
        end,
    NewPos = add_tuple2(Pos, NewDir),
    case outside_map(NewPos, MaxX, MaxY) of
            true -> [Pos] ++ Trail;
            false -> walk(NewPos, NewDir, MaxX, MaxY, Obstacles, [Pos] ++ Trail)
        end.

string_list_extents(Table) ->
    { length(hd(Table))-1, length(Table) -1 }.

do(File) ->
    code:add_path(".."),
    Map = helpers:read_file_of_string_list(File),
    Obstacles = helpers:find_coordinates_in_string_list(Map, ["#"], 0),
    [Guard | _] = helpers:find_coordinates_in_string_list(Map, ["\\^"], 0),
    {MaxX, MaxY} = string_list_extents(Map),
    Trail = walk(Guard, {0,-1}, MaxX, MaxY, Obstacles, []),
    length(lists:uniq(Trail)).