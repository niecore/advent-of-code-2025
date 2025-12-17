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
    Input = input:read_input(6),
    %% I split here Ops and Numbers because there
    %% was a problem with my transpose function
    %% because my input gets trimmed by the input
    %% module. I dont want to change the input
    %% for previous parts now.
    [OpsTokens | _ ] = lists:reverse(Input),
    Ops = string:tokens(OpsTokens, " "),

    %% the only difference in the transpose is that
    %% we transpose on single characters
    %% and not on tokens.
    Numbers = transpose(lists:droplast(Input)),

    %% Created nested lists based on empty columns
    Blocks = lists:foldr(fun(X, [H | T]) ->
        case string:trim(X) =:= "" of
            true -> [[], H | T];
            false -> [[X | H] | T]
        end
    end, [[]], Numbers),
    %% Trim strings and parse integers of nested lists
    BlocksIntegers = [[list_to_integer(string:trim(S)) || S <- Block] || Block <- Blocks],
    %% Readd Ops to nested lists
    BlocksWithOps = lists:zip(Ops, BlocksIntegers),

    Results = [case Operator of
        "+" -> lists:sum(X);
        "*" -> lists:foldl(fun(A, B) -> A * B end, 1, X);
        _ -> 0
    end || {Operator, X} <- BlocksWithOps],
    lists:sum(Results).


transpose([[] | _]) -> [];
transpose(Rows) ->
    [lists:map(fun hd/1, Rows) | transpose(lists:map(fun tl/1, Rows))].
