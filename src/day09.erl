-module(day09).
-export([test/0, part1/0, part2/0]).

test() ->
    25 = size({2,5}, {9,7}).

%% Returns a list of junction boxes
parse_input() ->
    Input = input:read_input(9),
    [ list_to_tuple([ list_to_integer(Cord) || Cord <- string:split(Line, ",", all)]) || Line <- Input].

size({X1, Y1}, {X2, Y2}) ->
    abs(X2 - X1 + 1) * abs(Y2 - Y1 + 1).

calculate_sizes(RedTiles) ->
    %% Calculate the distances between each pair of junction boxes in each circuit
    Pairing = [{size(A, B), A, B} || A <- RedTiles, B <- RedTiles, A < B],
    lists:keysort(1, Pairing).

part1() ->
    RedTiles = parse_input(),
    Sizes = calculate_sizes(RedTiles),
    {MaxSize, _A, _B} = BiggestCarpet = lists:last(Sizes),
    MaxSize.

part2() ->
    ok.
