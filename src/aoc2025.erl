-module(aoc2025).
-export([solve/2, test/1]).

solve(Day, Part) ->
    DayStr = lists:flatten(io_lib:format("~2..0B", [Day])),
    Module = list_to_atom("day" ++ DayStr),
    Function = list_to_atom("part" ++ integer_to_list(Part)),
    io:format("Solution Output: ~p~n", [Module:Function()]).

test(Day) ->
    DayStr = lists:flatten(io_lib:format("~2..0B", [Day])),
    Module = list_to_atom("day" ++ DayStr),
    Function = list_to_atom("test"),
    io:format("Test Output: ~p~n", [Module:Function()]).
