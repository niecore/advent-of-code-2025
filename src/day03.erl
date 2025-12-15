-module(day03).
-export([test/0, part1/0, part2/0]).

test() ->
    98 = calculate_joltage("987654321111111"),
    89 = calculate_joltage("811111111111119"),
    78 = calculate_joltage("234234234234278"),
    92 = calculate_joltage("818181911112111"),
    ok.

part1() ->
    BatteryPacks = input:read_input(3),
    BatteryPackJoltages = [calculate_joltage(BatteryPack) || BatteryPack <- BatteryPacks],
    lists:sum(BatteryPackJoltages).

part2() ->
    1.

calculate_joltage(BattaryPack) ->
    Pairs = pairs(input:digits(BattaryPack)),
    Joltages = [A * 10 + B || {A, B} <- Pairs],
    lists:max(Joltages).

pairs([]) -> [];
pairs([H | T]) ->
    [{H, X} || X <- T] ++ pairs(T).
