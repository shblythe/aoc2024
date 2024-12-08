-module(aoc8b).
-export([do/1]).
% -compile(export_all).

fold_pairs(_FunXYA, [_X], Accum) -> Accum;
fold_pairs(FunXYA, [X | Rest], Accum) -> fold_pairs(FunXYA, Rest, lists:foldl(fun (Y,A) -> FunXYA(X,Y,A) end, Accum, Rest)).

find_antinodes({X1, Y1}, {X2, Y2}, {DiffX, DiffY}, Bounds, Antinodes) ->
    case helpers:outside_map({X1-DiffX, Y1-DiffY}, Bounds) of
        false ->
            New = {X1 - DiffX, Y1 - DiffY},
            find_antinodes(New, {X2, Y2}, {DiffX, DiffY}, Bounds, Antinodes ++ [New]);
        true ->
            case helpers:outside_map({X2+DiffX, Y2+DiffY}, Bounds) of
                false ->
                    New = {X2 + DiffX, Y2 + DiffY},
                    find_antinodes({X1, Y1}, New, {DiffX, DiffY}, Bounds, Antinodes ++ [New]);
                true ->
                    Antinodes
            end
    end.

find_antinodes({X1, Y1, _F1}, {X2, Y2, _F2}, Bounds) ->
    DiffX = X2 - X1,
    DiffY = Y2 - Y1,
    [{X1, Y1}, {X2, Y2}] ++ find_antinodes({X1, Y1}, {X2, Y2}, {DiffX, DiffY}, Bounds, []).

%%% Returns a list of all antinode coordinates, as {X, Y}
find_antennae_antinodes(_Antennae, [], _Bounds, Antinodes) -> Antinodes;
find_antennae_antinodes(Antennae, [Frequency | RestFrequencies], Bounds, Antinodes) ->
    ThisFreqAntennae = lists:filter(fun ({_X, _Y, Freq}) -> Freq == Frequency end, Antennae),
    ThisFreqAntinodes = fold_pairs(fun(A1, A2, Accum) -> Accum ++ find_antinodes(A1, A2, Bounds) end, ThisFreqAntennae, []),
    find_antennae_antinodes(Antennae, RestFrequencies, Bounds, Antinodes ++ ThisFreqAntinodes).

do(File) ->
    code:add_path(".."),
    Map = helpers:read_file_of_string_list(File),
    Coords = helpers:find_coordinates_in_string_list(Map, ["[A-Za-z0-9]"], 0),
    Antennae = lists:map(fun ({X, Y}) -> {X, Y, helpers:get_element_from_string_list(Map, {X, Y})} end, Coords ),
    AntennaeFrequencies = lists:uniq(lists:map(fun ({_X, _Y, Type}) -> Type end, Antennae)),
    Bounds = helpers:string_list_extents(Map),
    Antinodes = find_antennae_antinodes(Antennae, AntennaeFrequencies, Bounds, []),
    AntinodesInBounds = lists:filter(fun(Coord) -> not helpers:outside_map(Coord, Bounds) end, Antinodes),
    length(lists:uniq(AntinodesInBounds)).