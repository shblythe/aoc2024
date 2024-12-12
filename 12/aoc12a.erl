-module(aoc12a).
-export([do/1]).
% -compile(export_all).

%%% Discover Region
%%% Work our way out from wherever we start in the region, searching UDLR for adjacent
%%% nodes in the same region, until we find no more.
%%% Return a Region, and a GardenMap bereft of the nodes in our new region, so they aren't found again
%%%
%%% Make a Region a tuple of { Id, #{Coords => 1, ...} }
%%% The value in the hash helps with perimeter calc later
discover_region({X, Y}, {Id, RegionCoords}, GardenMap) ->
    case maps:get({X, Y}, GardenMap, -1) == Id of
        false -> {{Id, RegionCoords}, GardenMap};
        true ->
            GardenMap1 = maps:remove({X, Y}, GardenMap),
            RegionCoords1 = maps:put({X, Y}, 1, RegionCoords),
            {{_, RegionCoords2}, GardenMap2} = discover_region({X-1, Y}, {Id, RegionCoords1}, GardenMap1),
            {{_, RegionCoords3}, GardenMap3} = discover_region({X+1, Y}, {Id, RegionCoords2}, GardenMap2),
            {{_, RegionCoords4}, GardenMap4} = discover_region({X, Y-1}, {Id, RegionCoords3}, GardenMap3),
            discover_region({X, Y+1}, {Id, RegionCoords4}, GardenMap4)
    end.

create_region_list(GardenMap, RegionList) ->
    case length(maps:keys(GardenMap)) of
        0 -> RegionList;
        _ ->
            Coords = hd(maps:keys(GardenMap)),
            Id = maps:get(Coords, GardenMap),
            {Region, NewGardenMap} = discover_region(Coords, {Id, #{}}, GardenMap),
            create_region_list(NewGardenMap, [Region | RegionList])
    end.

calc_perimeter_contribution({X, Y}, RegionCoords) ->
    4 -
    maps:get({X-1, Y}, RegionCoords, 0) -
    maps:get({X+1, Y}, RegionCoords, 0) -
    maps:get({X, Y-1}, RegionCoords, 0) -
    maps:get({X, Y+1}, RegionCoords, 0).

calc_perimeter(Region) ->
    lists:foldl(fun (Coords, Sum) -> Sum + calc_perimeter_contribution(Coords, Region) end,
                0,
                maps:keys(Region)).

add_vertex_hit(Coords, Vertices, Direction) -> maps:update_with(
    Coords,
    fun(Hits) -> sets:add_element(Direction, Hits) end,
    sets:add_element(Direction, sets:new()),
    Vertices).

%%% The number of vertices is the same as the number of sides.
%%% If we create a map of the coordinates of the vertices of each plot, and
%%% how often those are hit, those with an odd numbered count will either be a
%%% concave (3) or convex (1) vertex.
%%% For 2 hits, it could be two vertices in the same spot, if their sources are
%%% diagonally opposite, as in the below example:
%%% AAAAAA
%%% AAABBA
%%% AAABBA
%%% ABBAAA
%%% ABBAAA
%%% AAAAAA
%%% If it's hit from the same side twice (whether top, bottom, left or right) then
%%% it should be counted as zero, but if it's hit from top-left and bottom-right, or
%%% top-right and bottom-left, then it should count as two.
%%% Use atoms tl, tr, bl, br to indicate direction

%%% We'll use the same coordinate system, a vertex with coordinate {X,Y} is at
%%% the top left of the plot at {X,Y}.
calc_region_vertices(RegionCoords) ->
    %% Create a map of all the plot vertices with count
    RegionVertices = lists:foldl(
        fun (Coords, Vertices) ->
            {X, Y} = Coords,
            Vertices2 = add_vertex_hit({X, Y}, Vertices, tl),
            Vertices3 = add_vertex_hit({X+1, Y}, Vertices2, tr),
            Vertices4 = add_vertex_hit({X, Y+1}, Vertices3, bl),
            Vertices5 = add_vertex_hit({X+1, Y+1}, Vertices4, br),
            Vertices5
        end,
        #{},
        maps:keys(RegionCoords)),
    % io:format("Vertices: ~w~n", [RegionVertices]),
    maps:fold(
        fun(_, Hits, Sum) ->
            Sum + case sets:size(Hits) of
                1 -> 1;
                3 -> 1;
                4 -> 0;
                2 ->
                    TrBl = sets:from_list([tr,bl]),
                    case ((Hits == TrBl) or sets:is_disjoint(Hits, TrBl)) of
                        true -> 2;
                        false -> 0
                    end
            end
        end,
        0,
        RegionVertices
    ).

calc_fencing_cost_part1({_, RegionCoords}) ->
    length(maps:keys(RegionCoords)) * calc_perimeter(RegionCoords).

calc_fencing_cost_part2({_, RegionCoords}) ->
    length(maps:keys(RegionCoords)) * calc_region_vertices(RegionCoords).

do(File) ->
    code:add_path(".."),
    Map = helpers:read_file_of_string_list(File),
    Coords = helpers:find_coordinates_in_string_list(Map, ["[A-Z]"], 0),
    GardenMap = maps:from_list(
        lists:map(fun ({X, Y}) -> {{X, Y}, helpers:get_element_from_string_list(Map, {X, Y})} end, Coords )
    ),
    RegionList = create_region_list(GardenMap, []),
    % io:format("Regions: ~w~n",[lists:map(fun({Id, RegionCoords}) -> {
    %     Id,
    %     length(maps:keys(RegionCoords)),
    %     calc_perimeter(RegionCoords),
    %     calc_region_vertices(RegionCoords)
    % } end, RegionList)]),
    Part1 = lists:foldl(fun (Region, Sum) -> Sum + calc_fencing_cost_part1(Region) end, 0, RegionList),
    Part2 = lists:foldl(fun (Region, Sum) -> Sum + calc_fencing_cost_part2(Region) end, 0, RegionList),
    io:format("Part1: ~w~nPart2: ~w~n",[Part1, Part2]).
