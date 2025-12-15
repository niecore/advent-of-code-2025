-module(day03).
-export([test/0, part1/0, part2/0]).

test() ->
    99 = calculate_joltage(2,"7457548947854345576964379783795564264476647666477495455786784535589395464575557455353546464866378696"),
    98 = calculate_joltage(2,"987654321111111"),
    89 = calculate_joltage(2,"811111111111119"),
    78 = calculate_joltage(2,"234234234234278"),
    92 = calculate_joltage(2,"818181911112111"),
    987654321111 = calculate_joltage(12,"987654321111111"),
    811111111119 = calculate_joltage(12,"811111111111119"),
    434234234278 = calculate_joltage(12,"234234234234278"),
    888911112111 = calculate_joltage(12,"818181911112111"),
    ok.

part1() ->
    BatteryPacks = input:read_input(3),
    BatteryPackJoltages = [calculate_joltage(2,BatteryPack) || BatteryPack <- BatteryPacks],
    lists:sum(BatteryPackJoltages).

part2() ->
    BatteryPacks = input:read_input(3),
    BatteryPackJoltages = [calculate_joltage(12, BatteryPack) || BatteryPack <- BatteryPacks],
    lists:sum(BatteryPackJoltages).

calculate_joltage(Batteries, BattaryPack) ->
    Digits = input:digits(BattaryPack),
    Pairs = select_max_digits(Batteries, Digits, []),
    Joltage = lists:foldl(fun(X, Acc) -> Acc * 10 + X end, 0, Pairs),
    Joltage.

select_max_digits(0, _Remaining, Acc) ->
    lists:reverse(Acc);
select_max_digits(K, Digits, Acc) ->
    Len = length(Digits),
    %% Window: must leave at least K-1 digits after our pick
    WindowSize = Len - K + 1,
    Window = lists:sublist(Digits, WindowSize),
    {MaxDigit, MaxIdx} = max_with_index(Window),
    %% Take remaining digits after the chosen position
    Remaining = lists:nthtail(MaxIdx, Digits),
    select_max_digits(K - 1, Remaining, [MaxDigit | Acc]).

max_with_index([H | T]) ->
    %% 1 is the max indice
    %% 2 is the next indice
    %% H is the current max value
    max_with_index(T, H, 1, 2).
max_with_index([], MaxVal, MaxIdx, _Idx) ->
    %% base case
    {MaxVal, MaxIdx};
max_with_index([H | T], MaxVal, MaxIdx, Idx) when H > MaxVal ->
    %% Found new max value set
    %% new max value and indice
    max_with_index(T, H, Idx, Idx + 1);
max_with_index([_ | T], MaxVal, MaxIdx, Idx) ->
    %% current head is smaller than max value
    max_with_index(T, MaxVal, MaxIdx, Idx + 1).
