-module(day07).
-export([test/0, part1/0, part2/0]).

test() ->
    ok.

parse_input() ->
    Input = input:read_input(7),
    Manifold = [ [ map_ascii_to_enum(Square) || Square <- X ] || X <- Input].

calculate_next_state(Next, {Current, Splitted}) ->
    Combine = lists:zip3(lists:seq(1, length(Current)), Current, Next),

    %% calculate new splits
    NewSplitted = lists:sum([ 1 || {_, trace, splitter} <- Combine]),

    %% update tachyons
    NewTachyons = lists:flatmap(
        fun({Idx, Cur, Next}) ->
            case {Cur, Next} of
                {entry, empty} -> [{Idx, trace}];
                {trace, empty} -> [{Idx, trace}];
                {trace, splitter} -> [{Idx - 1, trace}, {Idx + 1, trace}];
                _ -> []
            end
        end,
        Combine
    ),

    %% create new row by combining current Row
    %% with new NewTachyons
    TachyonMap = maps:from_list(NewTachyons),
    NewRow = lists:zipwith(
        fun(Idx, NextSquare) ->
            maps:get(Idx, TachyonMap, NextSquare)
        end,
        lists:seq(1, length(Next)),
        Next
    ),
    {NewRow, NewSplitted + Splitted}.

map_ascii_to_enum($.) -> empty;
map_ascii_to_enum($S) -> entry;
map_ascii_to_enum($^) -> splitter;
map_ascii_to_enum($|) -> trace.

map_enum_to_ascii(empty) -> $.;
map_enum_to_ascii(entry) -> $S;
map_enum_to_ascii(splitter) -> $^;
map_enum_to_ascii(trace) -> $|.

print_manifold(Manifold) ->
    AsciiManifold = [ [ map_enum_to_ascii(X) || X <- Row ] || Row <- Manifold ],
    lists:foreach(fun(Row) -> io:format("~p~n", [Row]) end, AsciiManifold).

part1() ->
    Manifold = parse_input(),
    {_, Splitted} = lists:foldl(
        fun calculate_next_state/2,
        {hd(Manifold), 0},
        tl(Manifold)
    ),
    Splitted.

part2() ->
    ok.
