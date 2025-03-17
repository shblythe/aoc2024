-module(aoc15b).
-export([do/1]).
-compile(export_all).

is_obstacle(Pos, Boxes, Walls) ->
    PosLeft = move_transform($<, Pos),
    lists:member(Pos, Boxes) or lists:member(Pos, Walls) or
    lists:member(PosLeft, Boxes) or lists:member(PosLeft, Walls).

is_wall(Pos, Walls) ->
    PosLeft = move_transform($<, Pos),
    lists:member(Pos, Walls) or lists:member(PosLeft, Walls).

move_transform(Move, {PosX, PosY}) ->
    case Move of
        $< -> {PosX - 1, PosY};
        $> -> {PosX + 1, PosY};
        $^ -> {PosX, PosY - 1};
        $v -> {PosX, PosY + 1}
    end.

%%% Returns `CanMove` to indicate whether or not the item can move.
%%% Doesn't move anything.
check_move_obstacle(Move, Pos, Boxes, Walls) ->
    case is_wall(Pos, Walls) of
        true -> false;
        false ->
            RawNewPos = move_transform(Move, Pos),
            NextObjectPos = case lists:member(Move, "<>") of
                false -> RawNewPos;
                true -> move_transform(Move, RawNewPos)
            end,
            CanMove = case is_obstacle(NextObjectPos, Boxes, Walls) of
                false -> true;
                true -> check_move_obstacle(Move, NextObjectPos, Boxes, Walls)
            end,
            case lists:member(Move, "^v") of
                false -> CanMove;
                true ->
                    CanMove and begin
                        RawNewPosRight = move_transform($>, RawNewPos),
                        case is_obstacle(RawNewPosRight, Boxes, Walls) of
                            false -> true;
                            true -> check_move_obstacle(Move, RawNewPosRight, Boxes, Walls)
                        end
                    end
            end
    end.

%%% Returns { CanMove, Boxes } to indicate whether the pushing
%%% item can move or not, and new positions of boxes
move_obstacle(Move, Pos, Boxes, Walls) ->
    io:format("move_obstacle(~c, ~w, ~w, ...)~n", [Move, Pos, Boxes]),
    case lists:member(Pos, Walls) of
        true -> { false, Boxes };
        false ->
            RawNewPos = move_transform(Move, Pos),
            NextObjectPos = case lists:member(Move, "<>") of
                false -> RawNewPos;
                true -> move_transform(Move, RawNewPos)
            end,
            {CanMoveLeft, NewBoxesLeft} = case is_obstacle(NextObjectPos, Boxes, Walls) of
                false -> {true, Boxes};
                true -> move_obstacle(Move, NextObjectPos, Boxes, Walls)
            end,
            {CanMove, NewBoxes} = case CanMoveLeft and lists:member(Move, "^v") of
                false -> {CanMoveLeft, NewBoxesLeft};
                true ->
                    RawNewPosRight = move_transform($>, RawNewPos),
                    case is_obstacle(RawNewPosRight, Boxes, Walls) of
                        false -> {true, NewBoxesLeft};
                        true -> move_obstacle(Move, RawNewPosRight, Boxes, Walls)
                    end
            end,
            BoxToMove = case lists:member(Pos, NewBoxes) of
                true -> Pos;
                false -> move_transform($<, Pos)
            end,
            NewBoxPos = move_transform(Move, BoxToMove),
            io:format("Move box from ~w to ~w~n", [BoxToMove, NewBoxPos]),
            {
                CanMove,
                case CanMove of
                    true -> [NewBoxPos] ++ lists:delete(BoxToMove, NewBoxes);
                    false -> NewBoxes
                end
            }
    end.

%%% Returns {Robot, Boxes} indicating new positions
move_robot(Move, Robot, Boxes, Walls) ->
    RawNewPos = move_transform(Move, Robot),
    {CanMove, NewBoxes} = case is_obstacle(RawNewPos, Boxes, Walls) of
        false -> {true, Boxes};
        true -> case check_move_obstacle(Move, RawNewPos, Boxes, Walls) of
            false -> {false, Boxes};
            true -> move_obstacle(Move, RawNewPos, Boxes, Walls)
        end
    end,
    {
        case CanMove of
            true -> RawNewPos;
            false -> Robot
        end,
        NewBoxes
    }.

do_moves([], Robot, Boxes, _) -> {Robot, Boxes};
do_moves([Move | Rest], Robot, Boxes, Walls) ->
    {NewRobot, NewBoxes} = move_robot(Move, Robot, Boxes, Walls),
    io:format("Robot: ~w Boxes: ~w~n", [NewRobot, NewBoxes]),
    do_moves(Rest, NewRobot, NewBoxes, Walls).

coord_2x({X, Y}) -> {2*X, Y}.

do(File) ->
    code:add_path(".."),
    Input = helpers:read_file_of_string_list(File),
    Boxes = lists:map(fun coord_2x/1, helpers:find_coordinates_in_string_list(Input, ["O"], 0)),
    Walls = lists:map(fun coord_2x/1, helpers:find_coordinates_in_string_list(Input, ["#"], 0)),
    [Robot] = lists:map(fun coord_2x/1, helpers:find_coordinates_in_string_list(Input, ["@"], 0)),
    {match, RawDirections} = re:run(Input,"[<>v^]+",[global, {capture, all, list}]),
    Directions = lists:flatten(RawDirections),
    io:format("Robot: ~w Boxes: ~w~n", [Robot, Boxes]),
    {_, NewBoxes} = do_moves(Directions, Robot, Boxes, Walls),
    lists:foldl(fun({Bx, By}, Sum) -> Sum + 100*By + Bx end, 0, NewBoxes).