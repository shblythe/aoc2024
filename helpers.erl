-module(helpers).
-export([read_file_of_ints/1, split_2_columns/1]).

split_tokens(String) -> string:tokens(String, " \n\t").

tokens_to_ints(List) ->
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
    tokens_to_ints(split_tokens(binary_to_list(Contents))).
