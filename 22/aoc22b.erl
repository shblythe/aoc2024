-module(aoc22b).
-compile(export_all).

next(Num) ->
    Num1 = ((Num * 64) bxor Num) rem 16777216,
    Num2 = ((Num1 div 32) bxor Num1) rem 16777216,
    ((Num2 * 2048) bxor Num2) rem 16777216.

next_info(Num, Sequence) ->
    Next = next(Num),
    Price = Next rem 10,
    Change = Price - Num rem 10,
    NewSequence = [Change | lists:sublist(Sequence, 3)],
    {Next, Price, Change, NewSequence}.

find_best_sequences(_, 0, _, _, BestSequences) -> BestSequences;
find_best_sequences(Num, N, Sequence, Best, BestSequences) ->
    {Next, Price, _Change, NewSequence} = next_info(Num, Sequence),
    % io:format("~10w: ~w (~2w) ~w~n", [Next, Price, Change, NewSequence]),
    NewLengthOK = length(NewSequence) >= 4,
    NewBest = case (Price > Best) and NewLengthOK of
        true -> Price;
        false -> Best
    end,
    NewBestSequences = if
        NewLengthOK and (Price > Best) -> [NewSequence];
        NewLengthOK and (Price == Best) -> [NewSequence | BestSequences];
        true -> BestSequences
    end,
    find_best_sequences(Next, N-1, NewSequence, NewBest, NewBestSequences).
find_best_sequences(Num, N) -> find_best_sequences(Num, N, [], 0, []).

calc_price_with_sequence(_, 0, _, _) -> 0;
calc_price_with_sequence(Num, MaxN, TargetSequence, CurrentSequence) ->
    {Next, Price, _, Sequence} = next_info(Num, CurrentSequence),
    case Sequence == TargetSequence of
        true -> Price;
        false -> calc_price_with_sequence(Next, MaxN - 1, TargetSequence, Sequence)
    end.
calc_price_with_sequence(Num, MaxN, TargetSequence) ->
    Price = calc_price_with_sequence(Num, MaxN, TargetSequence, []),
    % io:format("~w ~w~n", [Num, Price]),
    Price.

calc_total_price_with_sequence(Numbers, Sequence, MaxN) ->
    lists:foldl(
        fun (Num, Sum) -> Sum + calc_price_with_sequence(Num, MaxN, Sequence) end,
        0, Numbers
    ).

calc_best_total_price_with_sequences(Numbers, Sequences, MaxN) ->
    lists:foldl(
        fun (Sequence, Best) ->
            Total = calc_total_price_with_sequence(Numbers, Sequence, MaxN),
            {BestTotal, BestSequence} = Best,
            NewBestTotal = max(Total, BestTotal),
            NewBestSequence = case Total > BestTotal of
                true ->
                    io:format("New best: ~10w ~w~n", [NewBestTotal, Sequence]),
                    Sequence;
                false -> BestSequence
            end,
            {NewBestTotal, NewBestSequence}
        end,
        {0, []},
        Sequences
    ).

find_all_best_sequences(Numbers, MaxN) ->
    lists:foldl(
        fun (Num, List) -> lists:concat([find_best_sequences(Num, MaxN), List]) end,
        [],
        Numbers
    ).

do(File) ->
    code:add_path(".."),
    Numbers = helpers:read_file_of_ints(File),
    BestSequences = find_all_best_sequences(Numbers, 2000),
    io:format("~w possible sequences~n", [length(BestSequences)]),
    UniqueBestSequences = lists:uniq(BestSequences),
    io:format("~w unique sequences~n", [length(UniqueBestSequences)]),
    calc_best_total_price_with_sequences(Numbers, UniqueBestSequences, 2000).