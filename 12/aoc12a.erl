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

calc_fencing_cost({_, RegionCoords}) ->
    length(maps:keys(RegionCoords)) * calc_perimeter(RegionCoords).

do(File) ->
    code:add_path(".."),
    Map = helpers:read_file_of_string_list(File),
    Coords = helpers:find_coordinates_in_string_list(Map, ["[A-Z]"], 0),
    GardenMap = maps:from_list(
        lists:map(fun ({X, Y}) -> {{X, Y}, helpers:get_element_from_string_list(Map, {X, Y})} end, Coords )
    ),
    RegionList = create_region_list(GardenMap, []),
    % io:format("~w~n",[lists:map(fun({Id, RegionCoords}) -> {
    %     Id,
    %     length(maps:keys(RegionCoords)),
    %     calc_perimeter(RegionCoords)
    % } end, RegionList)]),
    lists:foldl(fun (Region, Sum) -> Sum + calc_fencing_cost(Region) end, 0, RegionList).