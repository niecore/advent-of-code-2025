-module(aoc2025).
-export([solve/2, test/1]).

solve(Day, Part) ->
    DayStr = lists:flatten(io_lib:format("~2..0B", [Day])),
    Module = list_to_atom("day" ++ DayStr),
    Function = list_to_atom("part" ++ integer_to_list(Part)),

    StartTime = erlang:monotonic_time(microsecond),
    Result = Module:Function(),
    EndTime = erlang:monotonic_time(microsecond),
    ElapsedMs = (EndTime - StartTime) / 1000,

    io:format("Solution Output: ~p~n", [Result]),
    io:format("Execution Time: ~.3f ms~n", [ElapsedMs]).

test(Day) ->
    DayStr = lists:flatten(io_lib:format("~2..0B", [Day])),
    Module = list_to_atom("day" ++ DayStr),
    Function = list_to_atom("test"),
    io:format("Test Output: ~p~n", [Module:Function()]).
