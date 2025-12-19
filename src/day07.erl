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
    NewTachyons = get_splitted_tachyon(Current, Next),
    NewRow = get_updated_row(NewTachyons, Next),
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

%% calculates the indices of tachyons based on current and next row
get_splitted_tachyon(Current, Next) ->
    Combine = lists:zip3(lists:seq(1, length(Current)), Current, Next),

    %% update tachyons
    NewTachyons = lists:flatmap(
        fun({Idx, entry, empty}) -> [Idx];
            ({Idx, trace, empty}) -> [Idx];
            ({Idx, trace, splitter}) -> [Idx - 1, Idx + 1];
            (_) -> []
        end,
        Combine
    ).

%% calculates a new row from tachyon indices and raw next row
get_updated_row(Tachyons, Row) ->
    TachyonsSet = sets:from_list(Tachyons),
    [case sets:is_element(Idx, TachyonsSet) of
            true -> trace;
            false -> Elem
        end || {Idx, Elem} <- lists:enumerate(Row)].

part1() ->
    Manifold = parse_input(),
    {_, Splitted} = lists:foldl(
        fun calculate_next_state/2,
        {hd(Manifold), 0},
        tl(Manifold)
    ),
    Splitted.

part2() ->
    Manifold = parse_input(),
    %% does not compute without memorization
    ets:new(multiverses, [named_table, private, set]),
    solve_multiverse(Manifold).

solve_multiverse([Current]) -> 1;
solve_multiverse([Current | Manifold]) ->
    Next = hd(Manifold),
    NewTachyons = get_splitted_tachyon(Current, Next),

    PossiblePaths = lists:sum(lists:map(
        fun(NewTachyonIdx) ->
            NewRow = get_updated_row([NewTachyonIdx], Next),
            Lookup = {length(Manifold), NewTachyonIdx},
            case ets:lookup(multiverses, Lookup) of
                    [{Key, Result}] ->
                        Result;
                    [] ->
                        Result = solve_multiverse([NewRow | tl(Manifold)]),
                        ets:insert(multiverses, {Lookup, Result}),
                        Result
            end
        end,
        NewTachyons
    )),
    PossiblePaths.
