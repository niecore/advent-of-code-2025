-module(day08).
-export([test/0, part1/0, part2/0]).

test() ->
    ok.

%% Returns a list of junction boxes
parse_input() ->
    Input = input:read_input(8),
    [ list_to_tuple([ list_to_integer(Cord) || Cord <- string:split(Line, ",", all)]) || Line <- Input].

distance({X1, Y1, Z1}, {X2, Y2, Z2}) ->
    math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2) + math:pow(Z2 - Z1, 2)).

calculate_distances(JunctionBoxes) ->
    %% Calculate the distances between each pair of junction boxes in each circuit
    Pairing = [{distance(A, B), A, B} || A <- JunctionBoxes, B <- JunctionBoxes, A < B],
    lists:keysort(1, Pairing).


part1() ->
    JunctionBoxes = parse_input(),
    Distances = calculate_distances(JunctionBoxes),

    %% This is a List of all Circuits (List)
    Circuits = [ [Box] || Box <- JunctionBoxes],

    Connections = 1000,
    ConnectedCircuits = connections(Connections, Circuits, Distances),
    CircuitSizes = lists:sort([length(Circuit) || Circuit <- ConnectedCircuits]),
    BiggestCircuits = lists:nthtail(length(CircuitSizes) - 3, CircuitSizes),
    lists:foldl(fun(A, B) -> A * B end, 1, BiggestCircuits).

junctionboxes_in_the_same_circuit(Circuits, A, B) ->
    lists:any(fun(Circuit) -> lists:member(A, Circuit) andalso lists:member(B, Circuit) end, Circuits).

connections(0, Circuits, Distances) -> Circuits;
connections(N, Circuits, [ClosestJunctionBoxes | Rest]) ->
    {Distance, A, B} = ClosestJunctionBoxes,

    NewCircuits = case junctionboxes_in_the_same_circuit(Circuits, A, B) of
        true -> Circuits; %% dont link them
        false ->
            %% We already know that A and B are not in the same circuit (can be improved also to check the first element of partition to be size 1)
            %% So we have now a list of circuits that need be connected in the first element
            %% and all other circuits in the second element
            {[CircuitA, CircuitB | _], OtherCircuit} = lists:partition(fun(Circuit) -> lists:member(A, Circuit) orelse lists:member(B, Circuit) end, Circuits),
            [CircuitA ++ CircuitB] ++ OtherCircuit
    end,

    connections(N - 1, NewCircuits, Rest).

part2() ->
    JunctionBoxes = parse_input(),
    Distances = calculate_distances(JunctionBoxes),

    %% This is a List of all Circuits (List)
    Circuits = [ [Box] || Box <- JunctionBoxes],

    {{X1, _, _}, {X2, _, _}} = ConnectedCircuits = connections2(Circuits, Distances, {0, 0}),
    X1 * X2.

connections2(Circuits, [], Pair) -> cant_connect;
connections2(Circuits, _, Pair) when length(Circuits) == 1 -> Pair;
connections2(Circuits, [ClosestJunctionBoxes | Rest], _) ->
    {Distance, A, B} = ClosestJunctionBoxes,

    NewCircuits = case junctionboxes_in_the_same_circuit(Circuits, A, B) of
        true -> Circuits; %% dont link them
        false ->
            %% We already know that A and B are not in the same circuit (can be improved also to check the first element of partition to be size 1)
            %% So we have now a list of circuits that need be connected in the first element
            %% and all other circuits in the second element
            {[CircuitA, CircuitB | _], OtherCircuit} = lists:partition(fun(Circuit) -> lists:member(A, Circuit) orelse lists:member(B, Circuit) end, Circuits),
            [CircuitA ++ CircuitB] ++ OtherCircuit
    end,

    connections2(NewCircuits, Rest, {A, B}).
