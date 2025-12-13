-module(day02).
-export([test/0, part1/0, part2/0]).

test() ->
    true = is_invalid_part2([8,2,4,8,2,4,8,2,4], 1),
    [11, 22] = check_product_id_ranges([{11, 22}], fun is_invalid_part2/1),
    [99, 111] = check_product_id_ranges([{95, 115}], fun is_invalid_part2/1),
    [824824824] = check_product_id_ranges([{824824821,824824827}], fun is_invalid_part2/1),
    [[1,2]] = chunk_list([1, 2], 2),
    false = is_invalid_part2([1, 2], 1),
    true = is_invalid_part2([1, 1], 1),
    true = is_invalid_part2(111),
    true = is_invalid_part2(1212121212),
    true = is_invalid_part2(12341234),
    false = is_invalid_part2(12),
    true = is_invalid(11),
    true = is_invalid(1188511885),
    false = is_invalid(1),
    false = is_invalid(10),
    [11, 22] = [X || X <- lists:seq(11,22), is_invalid(X)],
    [1188511885] = [X || X <- lists:seq(1188511880,1188511890), is_invalid(X)],
    [11, 22, 1188511885] = check_product_id_ranges([{11, 22},{1188511880,1188511890}], fun is_invalid/1),
    ok.

part1() ->
    Input = parse_ranges(input:read_input(2)),
    InvalidIds = check_product_id_ranges(Input, fun is_invalid/1),
    lists:sum(InvalidIds).

part2() ->
    Input = parse_ranges(input:read_input(2)),
    InvalidIds = check_product_id_ranges(Input, fun is_invalid_part2/1),
    lists:sum(InvalidIds).

parse_ranges(Input) ->
    RangeStrings = string:split(string:trim(Input), ",", all),
    lists:map(fun(RangeStr) ->
        [Start, End] = string:split(RangeStr, "-"),
        {list_to_integer(Start), list_to_integer(End)}
    end, RangeStrings).

check_product_id_ranges(Ranges, Func) ->
    [N || {Start, End} <- Ranges, N <- lists:seq(Start, End), Func(N)].

is_invalid(Id) ->
    IdLength = trunc(math:floor(math:log10(Id) + 1)),
    case IdLength rem 2 of
        0 ->
            Divider = floor(math:pow(10, IdLength / 2)),
            Id rem Divider == Id div Divider;
        _ -> false %% length has to be even for this algorithm to work
    end.

chunk_list(List, Size) ->
    lists:foldr(fun(E, []) -> %% put first element in in nested list chunk
                        [[E]];
                    %% when the first list of acc (chunk) is not full
                    %% we add the new element to the the chunk as first element
                    %% note that we use foldr therefore its okay to put it first.
                    (E, [H | RAcc]) when length(H) < Size ->
                        [[E | H] | RAcc];
                    %% otherwise we create a new chunk with the element in it
                    (E, [H | RAcc]) ->
                        [[E], H | RAcc]
                end, [], List).

%% in this solution i switched to a string based approach instead of arithmetic as
%% in part 1 because working with rem and div might be possible by stacking pow 10 and
%% subtracting numbers of temporary number but this seems more complicated than using
%% a list approach where I can just check if the elements in the list are equal.
is_invalid_part2(Id) ->
    List = [C - $0 || C <- integer_to_list(Id)],
    is_invalid_part2(List, 1).
is_invalid_part2(List, ChunkSize) when length(List) / 2 < ChunkSize ->
    false;
is_invalid_part2(List, ChunkSize) when length(List) rem ChunkSize > 0 ->
    %% this calculation can be skipped because
    %% all chunks need to be same length
    is_invalid_part2(List, ChunkSize + 1);
is_invalid_part2(List, ChunkSize) ->
    Chunked = chunk_list(List, ChunkSize),
    IsSame = lists:all(fun(X) -> X == hd(Chunked) end, Chunked),

    case IsSame of
        true ->
            true;
        false ->
            is_invalid_part2(List, ChunkSize + 1)
    end.
