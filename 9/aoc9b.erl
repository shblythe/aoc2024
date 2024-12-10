-module(aoc9b).
-export([do/1]).
% -compile(export_all).

convert_to_disk_layout_space([], _Id, Surface) -> Surface;
convert_to_disk_layout_space([Length | RestMap], Id, Surface) ->
    convert_to_disk_layout_file(RestMap, Id, Surface ++ [{Length, -1}]).
convert_to_disk_layout_file([], _Id, Surface) -> Surface;
convert_to_disk_layout_file([Length | RestMap], Id, Surface) ->
    convert_to_disk_layout_space(RestMap, Id+1, Surface ++ [{Length, Id}]).
convert_to_disk_layout(Map) ->
    convert_to_disk_layout_file(Map, 0, []).

%%% Returns {Processed, Rest} where Rest is the updated structure of the area we
%%% attempted to move the file into, and Processed is the effect on the part we
%%% processed, which will either be a block representing the original file, if we
%%% failed, or a block representing the free space from where we moved it.
try_move_last_file(Area) ->
    [{FileLength, FileId} | RestArea] = lists:reverse(Area),
    {NoSpaceArea, SpaceArea } = lists:splitwith(
        fun ({BlockLength, BlockId}) -> (BlockId /= -1) or (BlockLength < FileLength) end,
        lists:reverse(RestArea)
    ),
    case length(SpaceArea) of
        0 -> {{FileLength, FileId}, NoSpaceArea};
        _ ->
            [{SpaceLength, -1} | RestSpaceArea] = SpaceArea,
            {
                {FileLength, -1},
                NoSpaceArea ++ [{FileLength, FileId}] ++ [{SpaceLength - FileLength, -1}] ++ RestSpaceArea
            }
    end.

strip_trailing_free_space(Surface) ->
    lists:reverse(lists:dropwhile(fun({_, Id}) -> Id == -1 end, lists:reverse(Surface))).

%%% defrag
%%% We need to keep track of what we've already processed and what we need to process.
%%% We're processing in reverse order, so we need to reverse the list first.
%%% When we try to move a file, we either need to add the file to what we've already processed
%%% or the free space to replace it.
%%% So that's what try_move_last_file needs to return, along with the remaining area still to be processed.
%%% Skip defragging free space
defrag_reversed([{Length, -1} | RestSurface]) -> [{Length, -1} | defrag_reversed(RestSurface)];
defrag_reversed([Block | RestSurface]) ->
    {Processed, Rest} = try_move_last_file(lists:reverse([Block | RestSurface])),
    [Processed | defrag_reversed(lists:reverse(Rest))];
defrag_reversed([]) -> [].

defrag(Surface) -> lists:reverse(defrag_reversed(lists:reverse(Surface))).

convert_to_flat_layout([{Length, Id} | Rest]) -> lists:duplicate(Length, Id) ++ convert_to_flat_layout(Rest);
convert_to_flat_layout([]) -> [].

checksum(Surface) -> lists:foldl(
    fun ({Pos, Id}, Sum) ->
        Sum + case Id of
            -1 -> 0;
            _ -> Pos*Id
        end
    end,
    0, lists:enumerate(0, Surface)).

do(File) ->
    code:add_path(".."),
    Map = lists:map(fun(A) -> A-48 end,helpers:read_file_of_string(File)),
    Layout = convert_to_disk_layout(Map),
    DefragLayout = defrag(Layout),
    FlatLayout = convert_to_flat_layout(DefragLayout),
    checksum(FlatLayout).