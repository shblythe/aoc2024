-module(aoc23a).
-compile(export_all).

find_all_connections([], ConnectionsMap) -> ConnectionsMap;
find_all_connections([{Node1, Node2} | Rest], ConnectionsMap) ->
    find_all_connections(Rest,
        maps:put(
            Node1,
            [Node2 | maps:get(Node1, ConnectionsMap, [])],
            maps:put(
                Node2,
                [Node1 | maps:get(Node2, ConnectionsMap, [])],
                ConnectionsMap)
        )
    ).

find_3nets_for_node(_Node, [], _ConnectionsMap, Nets) -> Nets;
find_3nets_for_node(Node, [ConnectedNode | Rest], ConnectionsMap, Nets) ->
    NodeConnections = sets:from_list(Rest),
    ConnectedNodeConnections = sets:from_list(maps:get(ConnectedNode, ConnectionsMap)),
    CommonConnections = sets:to_list(sets:intersection(NodeConnections, ConnectedNodeConnections)),
    % io:format("~s ~s ~s ~n", [Node, ConnectedNode, CommonConnections]),
    find_3nets_for_node(Node, Rest, ConnectionsMap,
        Nets ++ lists:map(fun (Common) -> lists:sort([Node, ConnectedNode, Common]) end, CommonConnections)
    ).

find_3nets([], _, Nets) -> Nets;
find_3nets([Node | Rest], ConnectionsMap, Nets) ->
    find_3nets(Rest, ConnectionsMap,
        find_3nets_for_node(Node, maps:get(Node, ConnectionsMap), ConnectionsMap, Nets)
    ).
find_3nets(ConnectionsMap) -> lists:uniq(find_3nets(maps:keys(ConnectionsMap), ConnectionsMap, [])).

do(File) ->
    code:add_path(".."),
    Input=helpers:read_file_of_string_list(File),
    Pairs = lists:map(
        fun (PairStr) ->
            Nodes = string:split(PairStr,"-"),
            {lists:nth(1, Nodes), lists:nth(2, Nodes)}
        end,
        Input
    ),
    ConnectionsMap = find_all_connections(Pairs, #{}),
    % io:format("~p~n", [ConnectionsMap]),
    Net3s = find_3nets(ConnectionsMap),
    Net3sWithT = lists:filter(
        fun (Net) -> lists:any(
            fun (Node) ->
                hd(Node) == $t
            end,
            Net
        )
        end,
        Net3s
    ),
    io:format("Part 1: ~w~n", [length(Net3sWithT)]).