-module(aoc2025).
-export([solve/2]).

solve(Day, Part) ->
    DayStr = lists:flatten(io_lib:format("~2..0B", [Day])),
    Module = list_to_atom("day" ++ DayStr),
    Function = list_to_atom("part" ++ integer_to_list(Part)),
    Module:Function().
