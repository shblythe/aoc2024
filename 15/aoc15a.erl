-module(aoc15a).
-export([do/1]).
-compile(export_all).

is_obstacle(Pos, Boxes, Walls) ->
    lists:member(Pos, Boxes) or lists:member(Pos, Walls).

move_transform(Move, {PosX, PosY}) ->
    case Move of
        $< -> {PosX - 1, PosY};
        $> -> {PosX + 1, PosY};
        $^ -> {PosX, PosY - 1};
        $v -> {PosX, PosY + 1}
    end.

%%% Returns { CanMove, Boxes } to indicate whether the pushing
%%% item can move or not, and new positions of boxes
move_obstacle(Move, Pos, Boxes, Walls) ->
    case lists:member(Pos, Walls) of
        true -> { false, Boxes };
        false ->
            RawNewPos = move_transform(Move, Pos),
            {CanMove, NewBoxes} = case is_obstacle(RawNewPos, Boxes, Walls) of
                false -> {true, Boxes};
                true -> move_obstacle(Move, RawNewPos, Boxes, Walls)
            end,
            {
                CanMove,
                case CanMove of
                    true -> [RawNewPos] ++ lists:delete(Pos, NewBoxes);
                    false -> NewBoxes
                end
            }
    end.

%%% Returns {Robot, Boxes} indicating new positions
move_robot(Move, Robot, Boxes, Walls) ->
    RawNewPos = move_transform(Move, Robot),
    {CanMove, NewBoxes} = case is_obstacle(RawNewPos, Boxes, Walls) of
        false -> {true, Boxes};
        true -> move_obstacle(Move, RawNewPos, Boxes, Walls)
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
    do_moves(Rest, NewRobot, NewBoxes, Walls).

do(File) ->
    code:add_path(".."),
    Input = helpers:read_file_of_string_list(File),
    Boxes = helpers:find_coordinates_in_string_list(Input, ["O"], 0),
    Walls = helpers:find_coordinates_in_string_list(Input, ["#"], 0),
    [Robot] = helpers:find_coordinates_in_string_list(Input, ["@"], 0),
    {match, RawDirections} = re:run(Input,"[<>v^]+",[global, {capture, all, list}]),
    Directions = lists:flatten(RawDirections),
    {_, NewBoxes} = do_moves(Directions, Robot, Boxes, Walls),
    lists:foldl(fun({Bx, By}, Sum) -> Sum + 100*By + Bx end, 0, NewBoxes).