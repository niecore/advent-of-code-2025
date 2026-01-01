-module(input).
-export([read_input/1, read_input_string/1, digits/1]).

read_input(Day) ->
    string:split(read_input_string(Day), "\n", all).

read_input_string(Day) ->
    Filename = filename:join([code:priv_dir(aoc2025), "inputs", io_lib:format("day~w.txt", [Day])]),
    case file:read_file(Filename) of
        {ok, Binary} ->
            Text = binary_to_list(Binary),
            string:trim(Text);
        {error, enoent} ->
            io:format("Warning: Input file not found: ~s~n", [Filename]),
            ""
    end.

digits(String) -> [C - $0 || C <- String].
