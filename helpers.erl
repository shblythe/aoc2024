-module(helpers).
-export([
    count_occurrences_in_string/2,
    print_string_list/1,
    read_file_of_ints/1,
    read_file_of_int_rows/1,
    read_file_of_string/1,
    split_2_columns/1,
    split_lines/1,
    string_list_to_ints/1,
    transpose/1]).

count_occurrences_in_string(Subject, Term) ->
    Result = re:run(Subject, "(" ++ Term ++ ")", [global, {capture,[1]}]),
    case Result of
        {match, Matches} -> length(Matches);
        _ -> 0
    end.

print_string_list([X | Rest]) ->
    io:format("~s~n", [X]),
    print_string_list(Rest);
print_string_list([]) -> ok.

split_tokens(String) -> string:tokens(String, " \n\t").

split_lines(String) -> string:tokens(String, "\n").

string_list_to_ints(List) ->
    lists:map(fun(X) -> {Int, _} = string:to_integer(X),
                        Int end,
              List).

split_2_columns([], AccL, AccR) -> [AccL, AccR];
split_2_columns(List, AccL, AccR) ->
    [L, R | Rest ] = List,
    split_2_columns(Rest, AccL ++ [L], AccR ++ [R]).

split_2_columns(List) -> split_2_columns(List, [], []).

read_file_of_ints(File) ->
    {ok, Contents} = file:read_file(File),
    string_list_to_ints(split_tokens(binary_to_list(Contents))).

read_file_of_int_rows(File) ->
    {ok, Contents} = file:read_file(File),
    Rows = split_lines(binary_to_list(Contents)),
    lists:map(fun(X) -> string_list_to_ints(split_tokens(X)) end, Rows).

read_file_of_string(File) ->
    {ok, Contents} = file:read_file(File),
    binary_to_list(Contents).

% From https://stackoverflow.com/questions/5389254/transposing-a-2-dimensional-matrix-in-erlang
% should work out how this works!
transpose([[]|_]) -> [];
transpose(M) -> [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].
