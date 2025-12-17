-module(day06).
-export([test/0, part1/0, part2/0]).

test() ->
    ok.

parse_input() ->
    Input = input:read_input(6),
    Parts = lists:map(fun(Line) ->
        string:tokens(Line, " ")
    end, Input).

part1() ->
    Homework = parse_input(),
    HomeworkTransposed = lists:map(fun lists:reverse/1, transpose(Homework)),
    HomeworkIntegers = [[Op | lists:map(fun list_to_integer/1, X)] || [Op | X ] <- HomeworkTransposed],
    Results = [case Operator of
        "+" -> lists:sum(X);
        "*" -> lists:foldl(fun(A, B) -> A * B end, 1, X);
        _ -> 0
    end || [Operator | X] <- HomeworkIntegers],
    lists:sum(Results).

part2() ->
    ok.

transpose([[] | _]) -> [];
transpose(Rows) ->
    [lists:map(fun hd/1, Rows) | transpose(lists:map(fun tl/1, Rows))].
