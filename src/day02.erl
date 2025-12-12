-module(day02).
-export([test/0, part1/0, part2/0]).

test() ->
    true = is_invalid(11),
    true = is_invalid(1188511885),
    false = is_invalid(1),
    false = is_invalid(10),
    [11, 22] = [X || X <- lists:seq(11,22), is_invalid(X)],
    [1188511885] = [X || X <- lists:seq(1188511880,1188511890), is_invalid(X)],
    [11, 22, 1188511885] = check_product_id_ranges([{11, 22},{1188511880,1188511890}]),
    ok.

part1() ->
    Input = parse_ranges(input:read_input(2)),
    InvalidIds = check_product_id_ranges(Input),
    lists:sum(InvalidIds).

part2() ->
    Input = input:read_input(2),
    is_invalid(11).

parse_ranges(Input) ->
    RangeStrings = string:split(string:trim(Input), ",", all),
    lists:map(fun(RangeStr) ->
        [Start, End] = string:split(RangeStr, "-"),
        {list_to_integer(Start), list_to_integer(End)}
    end, RangeStrings).

check_product_id_ranges(Ranges) ->
    [N || {Start, End} <- Ranges, N <- lists:seq(Start, End), is_invalid(N)].

is_invalid(Id) ->
    IdLength = trunc(math:floor(math:log10(Id) + 1)),
    case IdLength rem 2 of
        0 ->
            Divider = math:pow(10, IdLength / 2),
            DividerInt = floor(Divider),
            Id rem DividerInt == Id div DividerInt;
        _ -> false %% length has to be even for this algorithm to work
    end.
