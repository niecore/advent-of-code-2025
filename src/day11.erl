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

part2() ->
    ok.
