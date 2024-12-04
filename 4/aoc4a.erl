-module(aoc4a).
-export([do/1, diagonalise_left/1, diagonalise_right/1]).
%-compile(export_all).

count_occurrences_in_string(Subject) ->
    helpers:count_occurrences_in_string(Subject, "XMAS")
    + helpers:count_occurrences_in_string(Subject, "SAMX").

count_occurrences_in_string_list(List) ->
    lists:foldl(fun (Subject, Sum) -> Sum + count_occurrences_in_string(Subject) end, 0, List).

% Diagonalise the list to the right, by which I mean make
% abc          ..abc
% def  become  .def
% ghi          ghi
% Then this can be transposed and searched
% All I need to do is for each element add the number of dots corresponding to
% the length of the remaining elements
% 46 is ascii char for . - might be a better way
diagonalise_right_raw([X]) -> [X];
diagonalise_right_raw([X | Rest]) -> [lists:duplicate(length(Rest) ,46) ++ X] ++ diagonalise_right(Rest).

% Before we can transpose a diagonalised list, we have to ensure all the rows are the same length
% so fill to end
% ..abc         ..abc
% .def  becomes .def.
% ghi           ghi..
% Function assumes head of list is longest!
fill_to_end([], _) -> [];
fill_to_end([X | Rest], Length) -> [X ++ lists:duplicate(Length - length(X), 46)] ++ fill_to_end(Rest, Length).
fill_to_end(List) -> fill_to_end(List, length(hd(List))).

diagonalise_right(Table) -> fill_to_end(diagonalise_right_raw(Table)).

diagonalise_left(Table) -> lists:reverse(fill_to_end(diagonalise_right_raw(lists:reverse(Table)))).

do(File) ->
    code:add_path(".."),
    Contents = helpers:read_file_of_string(File),
    Table = helpers:split_lines(Contents),
    Horizontal = count_occurrences_in_string_list(Table),
    VerticalTable = helpers:transpose(Table),
    Vertical = count_occurrences_in_string_list(VerticalTable),
    DiagRightTable = helpers:transpose(diagonalise_right(Table)),
    DiagRight = count_occurrences_in_string_list(DiagRightTable),
    DiagLeftTable = helpers:transpose(diagonalise_left(Table)),
    DiagLeft = count_occurrences_in_string_list(DiagLeftTable),
    Horizontal+Vertical+DiagRight+DiagLeft.