-module(aoc24a).
-compile(export_all).

calc(Op, Arg1, Arg2) ->
    case Op of
        "AND" -> Arg1 band Arg2;
        "XOR" -> Arg1 bxor Arg2;
        "OR" -> Arg1 bor Arg2
    end.

process_connections([], [], ValuesMap) -> ValuesMap;
process_connections([], Unprocessed, ValuesMap) -> process_connections(Unprocessed, [], ValuesMap);
process_connections([Connection | Rest], Unprocessed, ValuesMap) ->
    {Arg1, Arg2, Op, Dst} = Connection,
    HaveBothValues = maps:is_key(Arg1, ValuesMap) andalso maps:is_key(Arg2, ValuesMap),
    if
        HaveBothValues ->
            process_connections(
                Rest,
                Unprocessed,
                maps:put(Dst, calc(Op, maps:get(Arg1, ValuesMap), maps:get(Arg2, ValuesMap)),
                ValuesMap));
        true -> process_connections(Rest, [Connection | Unprocessed], ValuesMap)
    end.
process_connections(Connections, ValuesMap) ->
    process_connections(Connections, [], ValuesMap).

do(File) ->
    code:add_path(".."),
    Input = helpers:read_file_of_string_list(File),
    {Values, RawConnections} = lists:partition(fun (Elem) -> string:find(Elem, ":") /= nomatch end, Input),
    ValuesMap = maps:from_list(
        lists:map(
            fun (ValueStr) ->
                [Label, Value] = helpers:split_tokens(ValueStr),
                {IntValue, _} = string:to_integer(Value),
                {Label, IntValue}
            end,
            Values
        )
    ),
    Connections = lists:map(
        fun (Connection) ->
            [Arg1, Op, Arg2, _, Dst] = helpers:split_tokens(Connection),
            {Arg1, Arg2, Op, Dst}
        end,
        RawConnections
    ),
    FinalValues = process_connections(Connections, ValuesMap),
    FinalZValues = lists:sort(
        fun ({A, _}, {B, _}) -> A < B end,
        lists:filter(
            fun ({Label, _}) -> hd(Label) == $z end,
            maps:to_list(FinalValues)
        )
    ),
    lists:sum(
        lists:map(
            fun ({Pow, {_, Value}}) -> Value * floor(math:pow(2,Pow)) end,
            lists:enumerate(0, FinalZValues)
        )
    ).