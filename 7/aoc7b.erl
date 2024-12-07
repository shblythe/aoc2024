-module(aoc7b).
-export([do/1]).
% -compile(export_all).

can_be_true(Answer, [X]) ->
    Answer == X;

can_be_true(Answer, [X | Rest]) ->
    % io:format("can_be_true: ~w ~w~n",[Answer, [X | Rest]]),
    (Answer >= 0) andalso (
        can_be_true(Answer - X, Rest) or
        ((Answer rem X == 0) and can_be_true(Answer div X, Rest)) or
        begin
            AnswerString = integer_to_list(Answer),
            TermString = integer_to_list(X),
            AnswerLength = length(AnswerString),
            TermLength = length(TermString),
            AnswerLength > TermLength andalso begin
                CheckTermString = string:sub_string(AnswerString, AnswerLength - TermLength + 1),
                % io:format("STR: ~s ~s ~s~n",[AnswerString, TermString, CheckTermString]),
                (TermString == CheckTermString) andalso begin
                    NewAnswerString = string:sub_string(AnswerString, 1, AnswerLength - TermLength),
                    {NewAnswer, _} = string:to_integer(NewAnswerString),
                    can_be_true(NewAnswer, Rest)
                end
            end
        end
    ).

can_be_true([Answer | Terms]) -> can_be_true(Answer, lists:reverse(Terms)).

do(File) ->
    code:add_path(".."),
    Equations = helpers:read_file_of_int_rows(File),
    Result = lists:foldl(
        fun (Equation, Sum) -> Sum + case can_be_true(Equation) of
                true -> hd(Equation);
                false -> 0
            end
        end,
        0,
        Equations
    ),
    io:format("Part 2: ~w~n", [Result]).