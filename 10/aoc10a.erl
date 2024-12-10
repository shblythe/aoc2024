-module(aoc10a).
-export([do/1]).
% -compile(export_all).

find_trailhead_summits({X, Y}, 9, TopoMap) ->
    case maps:get({X, Y}, TopoMap, -1) == 9 of
        true -> [{X, Y}];
        false -> []
    end;
find_trailhead_summits({X, Y}, Height, TopoMap) ->
    % io:format("~w ~w ~w~n",[X, Y, Height]),
    case maps:get({X, Y}, TopoMap, -1) == Height of
        false -> [];
        true ->
               find_trailhead_summits({X+1, Y}, Height + 1, TopoMap)
            ++ find_trailhead_summits({X-1, Y}, Height + 1, TopoMap)
            ++ find_trailhead_summits({X, Y+1}, Height + 1, TopoMap)
            ++ find_trailhead_summits({X, Y-1}, Height + 1, TopoMap)
    end.
count_trailhead_score({X, Y}, TopoMap) ->
    Summits = find_trailhead_summits({X, Y}, 0, TopoMap),
    {
        length(lists:uniq(Summits)),
        length(Summits)
    }.

do(File) ->
    code:add_path(".."),
    Map = helpers:read_file_of_string_list(File),
    Coords = helpers:find_coordinates_in_string_list(Map, ["[0-9]"], 0),
    TopoMap = maps:from_list(
        lists:map(fun ({X, Y}) -> {{X, Y}, helpers:get_element_from_string_list(Map, {X, Y}) - 48} end, Coords )
    ),
    {MaxX, MaxY} = helpers:string_list_extents(Map),
    {Part1, Part2} = lists:foldl(
        fun (X, AccX) ->
            helpers:add_tuple2(
                AccX,
                lists:foldl(
                    fun (Y, AccY) ->
                        Score = count_trailhead_score({X, Y}, TopoMap),
                        % io:format("~w ~w ~w~n",[X, Y, Score]),
                        helpers:add_tuple2(AccY, Score)
                    end,
                    {0, 0},
                    lists:seq(0, MaxY)
                )
            )
        end,
        {0, 0},
        lists:seq(0, MaxX)
    ),
    io:format("Part1 ~w~nPart2 ~w~n", [Part1, Part2]).