-module(aoc4b).
-compile(export_all).

find_occurrences_in_string(Subject, Term, PosOffset) ->
    Result = re:run(Subject, "(" ++ Term ++ ")", [global, {capture,[1]}]),
    case Result of
        {match, Matches} -> lists:map(fun ([{Pos,_}]) -> Pos+PosOffset end, Matches);
        _ -> []
    end.

% want to make this "Terms" so I can pass in "MAS" and "SAM", or maybe just presume that?
% could do Terms and then fold them together
find_occurrences_in_string_list(List, Term, PosOffset) ->
    lists:map(fun (String) -> find_occurrences_in_string(String, Term, PosOffset) end, List).

find_occurrences_in_string_list(Subject) ->
    find_occurrences_in_string_list(Subject, "MAS", 1).

print_string_list([X | Rest]) ->
    io:format("~s~n", [X]),
    print_string_list(Rest);
print_string_list([]) -> false.

do(File) ->
    code:add_path(".."),
    Contents = helpers:read_file_of_string(File),
    Table = helpers:split_lines(Contents),
    DiagRightTable = helpers:transpose(aoc4a:diagonalise_right(Table)),
    DiagRightHits = find_occurrences_in_string_list(DiagRightTable),
    DiagLeftTable = helpers:transpose(aoc4a:diagonalise_left(Table)),
    DiagLeftHits = find_occurrences_in_string_list(DiagLeftTable),
    print_string_list(DiagLeftTable),
    io:format("~w~n", [DiagLeftHits]),
    false.