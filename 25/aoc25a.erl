-module(aoc25a).
-compile(export_all).

split_7s([], NewList) -> NewList;
split_7s(List, NewList) ->
    {List7, Rest} = lists:split(7, List),
    split_7s(Rest, [List7 | NewList]).

try_key_with_lock(Key, Lock) ->
    Combined = lists:zipwith(fun (X, Y) -> (X + Y) > 5 end, Key, Lock),
    % io:format("~w ~w ~w~n", [Key, Lock, Combined]),
    case lists:any(fun (C) -> C end, Combined) of
        true -> 0;
        false -> 1
    end.

try_key_with_locks(_Key, [], Count) -> Count;
try_key_with_locks(Key, [Lock | Rest], Count) ->
    try_key_with_locks(Key, Rest, Count + try_key_with_lock(Key, Lock)).

try_pairs([], _Locks, Count) -> Count;
try_pairs([Key | Rest], Locks, Count) ->
    try_pairs(Rest, Locks, try_key_with_locks(Key, Locks, Count)).

try_pairs(Keys, Locks) -> try_pairs(Keys, Locks, 0).

do(File) ->
    code:add_path(".."),
    RawLines = helpers:read_file_of_string_list(File),
    Schematics = split_7s(RawLines, []),
    {RawLocks, RawKeys} = lists:partition(
        fun (Schematic) ->
            hd(Schematic) == "#####" end,
        Schematics
    ),
    Locks = lists:map(
        fun (Lock) ->
            lists:map(
                fun (Column) -> length(lists:takewhile(fun (A) -> A==$# end, Column))-1 end,
                helpers:transpose(Lock)
            )
        end,
        RawLocks
    ),
    Keys = lists:map(
        fun (Key) ->
            lists:map(
                fun (Column) -> 6-length(lists:takewhile(fun (A) -> A==$. end, Column)) end,
                helpers:transpose(Key)
            )
        end,
        RawKeys
    ),
    try_pairs(Keys, Locks).