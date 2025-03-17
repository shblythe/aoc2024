-module(aoc21a).
-compile(export_all).

numkey_to_dir(Start, Start, Dirs) -> Dirs;
numkey_to_dir(Start, Finish, Dirs) ->
    % io:format("numkey_to_dir ~c ~c~n", [Start, Finish]),
    case Start of
        $A ->
            if
                (Finish == $0) or (Finish rem 3 == 2) -> numkey_to_dir($0, Finish, Dirs ++ [$<]);
                true ->
                    Moves = (Finish - $0) div 3 + 1,
                    numkey_to_dir($0 + 3*Moves, Finish, Dirs ++ lists:duplicate(Moves,$^))
            end;
        $0 ->
            if
                (Finish == $A) or (Finish rem 3 == 0) -> numkey_to_dir($A, Finish, Dirs ++ [$>]);
                true -> numkey_to_dir($2, Finish, Dirs ++ [$^])
            end;
        _ ->
            case Finish of
                $A -> numkey_to_dir(Start, $3, Dirs) ++ [$v];
                $0 -> numkey_to_dir(Start, $2, Dirs) ++ [$v];
                _ ->
                    if
                        ((Finish - 1) rem 3) > ((Start - 1) rem 3) ->
                            numkey_to_dir(Start + 1, Finish, Dirs ++ [$>]);
                        ((Finish - 1) rem 3) < ((Start - 1) rem 3) ->
                            numkey_to_dir(Start - 1, Finish, Dirs ++ [$<]);
                        (Finish div 3) > (Start div 3) -> numkey_to_dir(Start + 3, Finish, Dirs ++ [$^]);
                        (Finish div 3) < (Start div 3) -> numkey_to_dir(Start - 3, Finish, Dirs ++ [$v])
                    end
            end
    end.

dir_keypad1(_, [], Out) -> Out;
dir_keypad1(NumStart, [In | InRest], Out) ->
    dir_keypad1(In, InRest, Out ++ numkey_to_dir(NumStart, In, []) ++ [$A]).

dir_to_dir(Start, Start) -> "";
dir_to_dir(Start, Finish) ->
    case {Start, Finish} of
        {$<, $v} -> ">";
        {$<, $>} -> ">>";
        {$<, $^} -> ">^";
        {$<, $A} -> ">>^";
        {$v, $<} -> "<";
        {$v, $>} -> ">";
        {$v, $^} -> "^";
        {$v, $A} -> "^>";
        {$>, $<} -> "<<";
        {$>, $v} -> "<";
        {$>, $^} -> "<^";
        {$>, $A} -> "^";
        {$^, $<} -> "v<";
        {$^, $v} -> "v";
        {$^, $>} -> "v>";
        {$^, $A} -> ">";
        {$A, $<} -> "v<<";
        {$A, $v} -> "v<";
        {$A, $>} -> "v";
        {$A, $^} -> "<"
    end.

dir_keypadn(_, [], Out) -> Out;
dir_keypadn(NumStart, [In | InRest], Out) ->
    dir_keypadn(In, InRest, Out ++ dir_to_dir(NumStart, In) ++ [$A]).

complexity(Code) ->
    DirKeypad1 = dir_keypad1($A, Code, []),
    % io:format("~s~n", [DirKeypad1]),
    DirKeypad2 = dir_keypadn($A, DirKeypad1, []),
    % io:format("~s~n", [DirKeypad2]),
    DirKeypad3 = dir_keypadn($A, DirKeypad2, []),
    {NumPart, _} = string:to_integer(lists:sublist(Code, 3)),
    io:format("~s ~w ~w ~s~n", [Code, length(DirKeypad3), NumPart, DirKeypad3]),
    length(DirKeypad3) * NumPart.

do(File) ->
    code:add_path(".."),
    Codes = helpers:read_file_of_string_list(File),
    lists:foldl(fun (Code, Sum) -> Sum + complexity(Code) end, 0, Codes).