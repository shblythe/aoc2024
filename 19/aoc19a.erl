-module(aoc19a).
-compile(export_all).

possible([], _, _) -> true;
possible(_, [], _) -> false;
possible(Design, [Towel | RestTowels], AllTowels) ->
    case erlang:get({'possible', Design, AllTowels}) of
        undefined ->
            % io:format("*> ~s ~s~n",[Design, Towel]),
            Result = case length(Towel) =< length(Design) of
                false -> possible(Design, RestTowels, AllTowels);
                true ->
                    { PossTowel , RestDesign } = lists:split(length(Towel), Design),
                    if
                        PossTowel < Towel -> false;
                        PossTowel == Towel ->
                            possible(RestDesign, AllTowels, AllTowels) orelse
                            possible(Design, RestTowels, AllTowels);
                        true -> possible(Design, RestTowels, AllTowels)
                    end
            end,
            erlang:put({'possible', Design, AllTowels}, Result),
            Result;
        Result ->
            % io:format("*< ~s ~s~n",[Design, Towel]),
            Result
    end.

do(File) ->
    code:add_path(".."),
    [TowelsInput | Designs] = helpers:read_file_of_string_list(File),
    Towels = lists:sort(helpers:split_tokens(TowelsInput)),
    helpers:print_string_list(Towels),
    io:format("-~n"),
    Results = lists:map(
        fun (Design) -> io:format("~s~n", [Design]), {Design, possible(Design, Towels, Towels)} end,
        Designs
    ),
    length(lists:filter(fun({_, Possible}) -> Possible end, Results)).
