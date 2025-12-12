-module(day01).
-export([test/0, part1/0, part2/0]).

test() ->
    ok.

part1() ->
    Input = input:read_input(1),
    rotate_dial(basic, 0, 50, Input).

part2() ->
    Input = input:read_input(1),
    rotate_dial(advanced, 0, 50, Input).

rotate_dial(Type, Password, Position, [Head | Tail]) ->
    Increment = case Head of
        "L" ++ N -> -list_to_integer(N);
        "R" ++ N -> list_to_integer(N)
    end,

    NewPosition = math_utils:mod(Position + Increment, 100),

    NewPassword = case Type of
        basic -> case NewPosition of
                0 -> 1;
                _ -> 0
        end;
        advanced -> case Increment of
            Increment when Increment > 0 -> (Position + Increment) div 100;
            Increment when Increment < 0 -> (((100 - Position) rem 100) + abs(Increment)) div 100;
            0 -> 0
        end
    end,

    rotate_dial(Type, Password + NewPassword, NewPosition, Tail);
rotate_dial(_, Password, _, []) ->
    io:format("Password ~p~n", [Password]),
    Password.
