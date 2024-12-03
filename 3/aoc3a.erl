-module(aoc3a).
-export([do/1]).

do(File) ->
    code:add_path(".."),
    Memory = helpers:read_file_of_string(File),
    {match, MulStrings} = re:run(Memory, "mul\\((\\d+),(\\d+)\\)", [global,{capture,[1,2],list}]),
    MulOperands=lists:map(fun helpers:string_list_to_ints/1, MulStrings),
    lists:foldl(
        fun(Pair, Sum) ->
                [X, Y] = Pair,
                X*Y + Sum end,
        0,
        MulOperands
        ).