-module(aoc9a).
-export([do/1]).
% -compile(export_all).

convert_to_disk_layout_space([], _Id, Surface) -> Surface;
convert_to_disk_layout_space([Length | RestMap], Id, Surface) ->
    convert_to_disk_layout_file(RestMap, Id, Surface ++ lists:duplicate(Length, -1)).
convert_to_disk_layout_file([], _Id, Surface) -> Surface;
convert_to_disk_layout_file([Length | RestMap], Id, Surface) ->
    convert_to_disk_layout_space(RestMap, Id+1, Surface ++ lists:duplicate(Length, Id)).
convert_to_disk_layout(Map) ->
    convert_to_disk_layout_file(Map, 0, []).

move_block([-1 | RestArea]) ->
    [Block | Rest] = lists:reverse(RestArea),
    [Block] ++ lists:reverse(Rest);
move_block(Area) -> Area.

strip_trailing_free_space(Surface) ->
    lists:reverse(lists:dropwhile(fun(A) -> A == -1 end, lists:reverse(Surface))).

%%% When we defrag, we can ignore space at the end and let the Surface get smaller.
%%% It doesn't contribute to the checksum.
%%% In fact, the non-existence of free space in our Surface is the easiest way to tell
%%% we've finished.
defrag(Surface) ->
    {DoneArea, FragArea} = lists:splitwith(fun (A) -> A /= -1 end, strip_trailing_free_space(Surface)),
    % io:format("~w ~w~n", [DoneArea, FragArea]),
    case length(FragArea) == 0 of
        false -> defrag(DoneArea ++ move_block(FragArea));
        true -> DoneArea
    end.

checksum(Surface) -> lists:foldl(fun ({Pos, Id}, Sum) -> Sum + Pos*Id end, 0, lists:enumerate(0, Surface)).

do(File) ->
    code:add_path(".."),
    Map = lists:map(fun(A) -> A-48 end,helpers:read_file_of_string(File)),
    Layout = convert_to_disk_layout(Map),
    DefragLayout = defrag(Layout),
    checksum(DefragLayout).