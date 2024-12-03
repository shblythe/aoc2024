-module(aoc3b).
-export([do/1]).

do(File) ->
    code:add_path(".."),
    Memory = helpers:read_file_of_string(File),
    CleanMemory = re:replace(Memory, "\n", "", [global]),
    %% Find all of the sections that either start with the beginning of the string, or "do()" or "don't()"
    %% and end before the next do() or don't() or end of string.
    {match, OpStrings} = re:run(CleanMemory, "(?>(^|do\\(\\)|don't\\(\\))(.*?)(?=(?>do\\(\\)|don't\\(\\)|$)))",
                                [global,{capture,[1,2],list}]),
    %% Throw away the ones that start with "don't()" and drop the operators
    %% We're treating no op (start of string) as do()
    DoOps = lists:filtermap(
        fun([X,Y]) ->
            case X of
                "don't()" -> false;
                _ -> {true, Y}
            end
        end,
        OpStrings),
    lists:foldl(
        fun(OpString, Sum) ->
            {match, MulStrings} = re:run(OpString, "mul\\((\\d+),(\\d+)\\)", [global,{capture,[1,2],list}]),
            MulOperands=lists:map(fun helpers:string_list_to_ints/1, MulStrings),
            Sum + lists:foldl(
                fun(Pair, MulSum) ->
                        [X, Y] = Pair,
                        X*Y + MulSum end,
                0,
                MulOperands
                )
            end,
            0,
            DoOps
        ).

