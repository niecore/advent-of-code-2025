-module(input).
-export([read_input/1]).

read_input(Day) ->
    Filename = filename:join([code:priv_dir(aoc2025), "inputs", io_lib:format("day~w.txt", [Day])]),
    case file:read_file(Filename) of
        {ok, Binary} ->
            Text = binary_to_list(Binary),
            string:split(string:trim(Text), "\n", all);
        {error, enoent} ->
            io:format("Warning: Input file not found: ~s~n", [Filename]),
            ""
    end.
