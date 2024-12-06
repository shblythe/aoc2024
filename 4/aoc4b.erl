-module(aoc4b).
-export([do/1]).
% -compile(export_all).

find_occurrences_in_string_list(Subject) ->
    helpers:find_occurrences_in_string_list(Subject, ["MAS", "SAM"], 1).

do(File) ->
    code:add_path(".."),
    Table = helpers:read_file_of_string_list(File),
    DiagRightTable = helpers:transpose(aoc4a:diagonalise_right(Table)),
    % Get the table of coordinates with MAS or SAM on the right diagonal, converting the coordinates back to original table
    % Basically:
    %   - get the list of lists of search hits, one list for each row
    %   - enumerate the list of lists of hits, so we have a list of {row, [hits]} tuples
    %   - map each element of that to a new list of tuples of {X, Y} mapping back to the original coordinates
    %   - flatten the list, to give a list of tuples of coordinates at which we find MAS or SAM on the right diagonal
    DiagRightHits = lists:flatten(lists:map(
                        fun ({Y, Xs}) -> lists:foldl(
                                           fun (X, Coords) -> Coords ++ [{X+1-(length(hd(Table)) - Y), X}] end,
                                           [],
                                           Xs
                            ) end,
                        lists:enumerate(0,find_occurrences_in_string_list(DiagRightTable))
                    )),
    DiagLeftTable = helpers:transpose(aoc4a:diagonalise_left(Table)),
    % Get the table of coordinates with MAS or SAM on the left diagonal, converting the coordinates back to original table
    DiagLeftHits = lists:flatten(lists:map(
                        fun ({Y, Xs}) -> lists:foldl(
                                           fun (X, Coords) -> Coords ++ [{Y-X, X}] end,
                                           [],
                                           Xs
                            ) end,
                        lists:enumerate(0,find_occurrences_in_string_list(DiagLeftTable))
                    )),
    % Find the intersection of the two lists, that's where they cross, and get the size
    length([C1 || C1 <- DiagRightHits, C2 <- DiagLeftHits, C1 == C2]).