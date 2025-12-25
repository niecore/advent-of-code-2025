-module(day09).
-export([test/0, part1/0, part2/0]).

test() ->
    24 = size({2,5}, {9,7}),
    ok.

parse_input() ->
    Input = input:read_input(9),
    [ list_to_tuple([ list_to_integer(Cord) || Cord <- string:split(Line, ",", all)]) || Line <- Input].

size({X1, Y1}, {X2, Y2}) ->
    (abs(X1 - X2) + 1) * (abs(Y1 - Y2) + 1).

calculate_sizes(RedTiles) ->
    Pairing = [{size(A, B), A, B} || A <- RedTiles, B <- RedTiles, A < B],
    lists:keysort(1, Pairing).

part1() ->
    RedTiles = parse_input(),
    Sizes = calculate_sizes(RedTiles),
    {MaxSize, _A, _B} = BiggestCarpet = lists:last(Sizes),
    MaxSize.

point_is_in_polygon(Edges, {X, Y}) ->
    %% raytrace to the right
    %%
    %% If the ray intersects an odd number of edges, the point is inside the polygon
    %% If the ray intersects an even number of edges, the point is outside the polygon
    Count = lists:foldl(fun({{X1, Y1}, {X2, Y2}}, Acc) ->
        if
            %% Edge is vertical
            X1 =:= X2 andalso X < X1 andalso min(Y1, Y2) =< Y andalso Y =< max(Y1, Y2) ->
                Acc + 1;
            %% Edge is horizontal
            Y1 =:= Y2 andalso Y =:= Y1 andalso X < min(X1, X2) ->
                Acc + 1;
            true ->
                Acc
        end
    end, 0, Edges),
    Count rem 2 =:= 1.

list_points_in_edge({{X1, Y}, {X2, Y}}) ->
    [{X, Y} || X <- lists:seq(min(X1, X2), max(X1, X2))];
list_points_in_edge({{X, Y1}, {X, Y2}}) ->
    [{X, Y} || Y <- lists:seq(min(Y1, Y2), max(Y1, Y2))].

init_floor_ets(Edges) ->
    ets:new(floor, [named_table, private, set]),
    Points = lists:flatmap(fun list_points_in_edge/1, Edges),
    lists:foreach(fun(Point) -> ets:insert(floor, {Point, true}) end, Points).

part2() ->
    %% takes ~10 Minutes to get the Result
    Polygons = parse_input(),
    Squares = lists:reverse(
        %% sort squares by biggest first
        lists:keysort(1,
            [{size(A, B), A, B} || A <- Polygons, B <- Polygons, A > B]
        )
    ),
    Edges = lists:zip(Polygons, tl(Polygons) ++ [hd(Polygons)]),
    init_floor_ets(Edges),

    lists:search(fun({Size, {X1, Y1}, {X2, Y2}}) ->
        SquareEdges = [
            {{X1, Y1}, {X1, Y2}},
            {{X2, Y1}, {X2, Y2}},
            {{X1, Y1}, {X2, Y1}},
            {{X1, Y2}, {X2, Y2}}
        ],

        SquarePoints = lists:flatmap(fun list_points_in_edge/1, SquareEdges),

        lists:all(fun(Point) ->
            case ets:lookup(floor, Point) of
                [{_, Result}] ->
                    Result;
                [] ->
                    InPolygon = point_is_in_polygon(Edges, Point),
                    %% here i could optimize the future lookup by finding the next
                    %% edges in all directions (up, down, left, right) and set
                    %% all points also in the floor table
                    ets:insert(floor, {Point, InPolygon}),
                    InPolygon

            end
        end, SquarePoints)
    end, Squares).
