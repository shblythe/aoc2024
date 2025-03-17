-module(aoc20a).
-compile(export_all).

all_dirs() -> [{-1,0},{1,0},{0,-1},{0,1}].

cheat(_, _, _, [], _, _, Routes) -> Routes;
cheat({CurrentX, CurrentY}, Last, EndPos, [{OffsetX, OffsetY} | RestOffsets], Path, Count, Routes) ->
    io:format("Cheating from ~w ~w~n", [{CurrentX, CurrentY}, {OffsetX, OffsetY}]),
    CheatPos = {CurrentX+OffsetX, CurrentY+OffsetY},
    CheatPosOnPath = maps:get(CheatPos, Path, false),
    case (CheatPos /= Last andalso not CheatPosOnPath) of
        false ->
            % io:format("Looking for next cheat ~w~n", [RestOffsets]),
            cheat({CurrentX, CurrentY}, Last, EndPos, RestOffsets, Path, Count, Routes);
        true ->
            % io:format("Cheating at ~w~n", [CheatPos]),
            % PROBLEM IS HERE!  There can be two possible paths out of cheats!
            NextPossiblesAfterCheat = find_nexts_on_paths(CheatPos, {CurrentX, CurrentY}, all_dirs(), Path, []),
            lists:flatten(
                lists:map(
                    fun(NextPosAfterCheat) ->
                        case NextPosAfterCheat of
                            deadend -> cheat({CurrentX, CurrentY}, Last, EndPos, RestOffsets, Path, Count, []);
                            Next ->
                                % io:format("Calling with ~w~n", [Next]),
                                % io:format("Rest offsets are ~w~n", [RestOffsets]),
                                find_routes(Next, CheatPos, EndPos, Path, Count+2,
                                    cheat({CurrentX, CurrentY}, Last, EndPos, RestOffsets, Path, Count, []),
                                    true)
                        end
                    end,
                    NextPossiblesAfterCheat
                )
            ) ++ Routes
    end.

find_nexts_on_paths(_, _, [], _, Nexts) -> Nexts;
find_nexts_on_paths({CurrentX, CurrentY}, Last, [{OffsetX, OffsetY} | RestOffsets], Path, Nexts) ->
    New = {CurrentX+OffsetX, CurrentY+OffsetY},
    NewOnPath = maps:get(New, Path, false),
    find_nexts_on_paths({CurrentX, CurrentY}, Last, RestOffsets, Path,
        case (New /= Last andalso NewOnPath) of
            true -> [New | Nexts];
            false -> Nexts
        end
    ).

find_next_on_path(_, _, [], _) -> deadend;
find_next_on_path({CurrentX, CurrentY}, Last, [{OffsetX, OffsetY} | RestOffsets], Path) ->
    New = {CurrentX+OffsetX, CurrentY+OffsetY},
    NewOnPath = maps:get(New, Path, false),
    case (New /= Last andalso NewOnPath) of
        true -> New;
        false -> find_next_on_path({CurrentX, CurrentY}, Last, RestOffsets, Path)
    end.

find_routes(EndPos, _, EndPos, _, Count, Routes, _) -> [Count | Routes];
find_routes(deadend, _, _, _, _, Routes, _) -> Routes;
find_routes(Current, Last, EndPos, Path, Count, Routes, Cheated) ->
    % io:format("~w ~w~n",[Current, EndPos]),
    NextPos = find_next_on_path(Current, Last, all_dirs(), Path),
    RoutesFromHere = find_routes(NextPos, Current, EndPos, Path, Count+1, Routes, Cheated),
    case Cheated of
        true -> RoutesFromHere;
        false -> cheat(Current, Last, EndPos, all_dirs(), Path, Count, RoutesFromHere ++ Routes)
    end.

find_route(EndPos, _, EndPos, _, Count) -> Count;
find_route(Current, Last, EndPos, Path, Count) ->
    io:format("~w ~w~n",[Current, EndPos]),
    NextPos = find_next_on_path(Current, Last, all_dirs(), Path),
    find_route(NextPos, Current, EndPos, Path, Count+1).

do(File) ->
    code:add_path(".."),
    Input = helpers:read_file_of_string_list(File),
    [StartPos] = helpers:find_coordinates_in_string_list(Input, ["S"], 0),
    [EndPos] = helpers:find_coordinates_in_string_list(Input, ["E"], 0),
    RawPath = [EndPos | helpers:find_coordinates_in_string_list(Input, ["\\."], 0)],
    Path = maps:from_list(lists:map(fun(Coord) -> {Coord, true} end, RawPath)),
    [StandardLength] = find_routes(StartPos, StartPos, EndPos, Path, 0, [], true),
    Routes = find_routes(StartPos, StartPos, EndPos, Path, 0, [], false),
    lists:sort(lists:filtermap(
        fun (Length) ->
            case Length < StandardLength of
                true -> {true, StandardLength - Length};
                false -> false
            end
        end,
        Routes
    )).
