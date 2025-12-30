-module(day11).
-export([test/0, part1/0, part2/0]).

-record(machine, {light, buttons, joltages}).

test() ->
    ok.

parse_input() ->
    Input = input:read_input(11),
    parse_lines(Input, #{}).

parse_lines([], Acc) ->
    Acc;
parse_lines([Line | Rest], Acc) ->
    [Key, ValuesPart] = string:split(Line, ": ", leading),
    Values = string:split(ValuesPart, " ", all),
    parse_lines(Rest, Acc#{Key => Values}).

part1() ->
    Input = parse_input(),
    length(find_path(Input, "you", [])).

find_path(Input, "out", Visited) ->
    [lists:reverse(["out" | Visited])];
find_path(Input, Current, Visited) ->
    case maps:get(Current, Input) of
        Neighbors ->
            lists:flatmap(fun(Neighbor) ->
                find_path(Input, Neighbor, [Current | Visited])
            end, Neighbors)
    end.

count_paths(Input, "out", {true, true}) ->
    1;
count_paths(Input, "out", _) ->
    0;
count_paths(Input, Current, {VisitedDac, VisitedFft}) ->
    Dac = case Current =:= "dac" of
        true -> true;
        false -> VisitedDac
    end,
    Fft = case Current =:= "fft" of
        true -> true;
        false -> VisitedFft
    end,
    Lookup = {Current, Dac, Fft},
    case ets:lookup(wires, Lookup) of
        [{_, Count}] ->
            Count;
        [] ->
            case maps:get(Current, Input) of
                Neighbors ->
                    Count = lists:sum(lists:map(fun(Neighbor) ->
                        count_paths(Input, Neighbor, {Dac, Fft})
                    end, Neighbors)),
                    ets:insert(wires, {Lookup, Count}),
                    Count
            end
    end.

part2() ->
    Input = parse_input(),
    ets:new(wires, [named_table, private, set]),
    count_paths(Input, "svr", {false, false}).
