-module(aoc17b).
-export([do/1]).
-compile(export_all).

print_state(RA,RB,RC,Prg) ->
    io:format("A: ~w B: ~w C: ~w~n",[RA,RB,RC]),
    io:format("Prg: ~w~n",[Prg]).

combo(Oper, RA, RB, RC) ->
    case Oper of
        0 -> 0;
        1 -> 1;
        2 -> 2;
        3 -> 3;
        4 -> RA;
        5 -> RB;
        6 -> RC
    end.

dv(Oper, RA, RB, RC) ->
    RA div trunc(math:pow(2, combo(Oper, RA, RB, RC))).

adv(Oper, RA, RB, RC, PC, Out) ->
    [dv(Oper, RA, RB, RC), RB, RC, PC, Out].

bdv(Oper, RA, RB, RC, PC, Out) ->
    [RA, dv(Oper, RA, RB, RC), RC, PC, Out].

cdv(Oper, RA, RB, RC, PC, Out) ->
    [RA, RB, dv(Oper, RA, RB, RC), PC, Out].

bxl(Oper, RA, RB, RC, PC, Out) ->
    [RA, RB bxor Oper, RC, PC, Out].

bst(Oper, RA, RB, RC, PC, Out) ->
    RB1 = combo(Oper, RA, RB, RC) rem 8,
    [RA, RB1, RC, PC, Out].

jnz(Oper, RA, RB, RC, PC, Out) ->
    case RA of
        0 -> [RA, RB, RC, PC, Out];
        _ -> [RA, RB, RC, Oper, Out]
    end.

bxc(_Oper, RA, RB, RC, PC, Out) ->
    [RA, RB bxor RC, RC, PC, Out].

out(Oper, RA, RB, RC, PC, Out) ->
    [RA, RB, RC, PC, Out ++ [combo(Oper, RA, RB, RC) rem 8]].

run(RA, RB, RC, PC, Prg, Out) ->
    case PC >= length(Prg) of
        true -> Out;
        false ->
            Opc = lists:nth(PC+1, Prg),
            Oper = lists:nth(PC+2, Prg),
            [RA1, RB1, RC1, PC1, Out1] = case Opc of
                0 -> adv(Oper, RA, RB, RC, PC, Out);
                1 -> bxl(Oper, RA, RB, RC, PC, Out);
                2 -> bst(Oper, RA, RB, RC, PC, Out);
                3 -> jnz(Oper, RA, RB, RC, PC, Out);
                4 -> bxc(Oper, RA, RB, RC, PC, Out);
                5 -> out(Oper, RA, RB, RC, PC, Out);
                6 -> bdv(Oper, RA, RB, RC, PC, Out);
                7 -> cdv(Oper, RA, RB, RC, PC, Out)
            end,
            PC2 = case PC == PC1 of
                true -> PC + 2;
                false -> PC1
            end,
            run(RA1, RB1, RC1, PC2, Prg, Out1)
    end.

% Returns count of how many list terms are the same
compare_lists([], [], Count) -> Count;
compare_lists([X | Rest1], [X | Rest2], Count) -> compare_lists(Rest1, Rest2, Count+1);
compare_lists(_, _, Count) -> Count.

find_a(Prg, TryA, Count) ->
    Out = run(TryA, 0, 0, 0, Prg, []),
    io:format("~w ~w ~w ~w/~w~n", [Count, TryA, Out, length(Out), length(Prg)]),
        case Out == Prg of
            true -> {TryA, Out};
            false ->
                NewA = case length(Out) < length(Prg) of
                    true -> TryA*3*trunc(math:pow(8,length(Prg)-1));
                    false ->
                        CorrectTerms = compare_lists(lists:reverse(Out), lists:reverse(Prg), 0),
                        TryA+trunc(math:pow(8,length(Prg)-CorrectTerms-1))
                end,
                find_a(Prg, NewA, Count+1)
    end.

do(File) ->
    code:add_path(".."),
    [[_,_,RA],[_,_,RB],[_,_,RC],[_|Prg]] = helpers:read_file_of_int_rows(File),
    print_state(RA,RB,RC,Prg),
    find_a(Prg, 1,0).