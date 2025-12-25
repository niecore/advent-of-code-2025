-module(day10).
-export([test/0, part1/0, part2/0]).

-record(machine, {light, buttons, joltages}).

test() ->
    [true] = press_button([false], [0]),
    [false] = press_button([false], [2]),
    ok.

parse_light_string(Str) ->
    Trimmed = string:trim(Str, both, "[]"),
    lists:map(fun(C) -> C =:= $# end, Trimmed).

parse_button_string(Str) ->
    Trimmed = string:trim(Str, both, "()"),
    Tokens = string:tokens(Trimmed, ","),
    lists:map(fun(S) -> list_to_integer(string:trim(S)) end, Tokens).

parse_machine(Str) ->
    Tokens = string:tokens(Str, " "),
    LightString = hd(Tokens),
    Light = parse_light_string(LightString),
    ButtonsStrings = lists:sublist(Tokens, 2, length(Tokens) - 2),
    Buttons = [parse_button_string(Str) || Str <- ButtonsStrings],
    JoltagesString = lists:last(Tokens),
    #machine{light = Light, buttons = Buttons, joltages = JoltagesString}.

press_button(Light, Button) ->
    %% returns a new light
    lists:map(
        fun({I, C}) ->
            case lists:member(I-1, Button) of
                true -> not C;
                false -> C
            end
        end,
        lists:enumerate(Light)
    ).

turn_on_machine(Machine) ->
    InitialLight = [false || _ <- Machine#machine.light],
    turn_on_machine(
        sets:from_list([InitialLight]),
        [{InitialLight, 0}],
        Machine#machine.light,
        Machine#machine.buttons
    ).

turn_on_machine(_, [{Light, Iteration} | _ ], Light, _) ->
    Iteration;
turn_on_machine(KnownLights, [{CurrentLight, Iteration} | RestQueue ], TargetLight, Buttons) ->
    NewLights = [press_button(CurrentLight, Button) || Button <- Buttons],
    NewLightsFiltered = [Light || Light <- NewLights, not sets:is_element(Light, KnownLights)],
    NewLightsWithIteration = [{Light, Iteration + 1} || Light <- NewLightsFiltered],

    turn_on_machine(
        sets:union(KnownLights, sets:from_list(NewLightsFiltered)),
        RestQueue ++ NewLightsWithIteration,
        TargetLight,
        Buttons
    ).

part1() ->
    Input = input:read_input(10),
    Machines = lists:map(fun parse_machine/1, Input),
    lists:sum(lists:map(fun turn_on_machine/1, Machines)).

part2() ->
    Input = input:read_input(10),
    ok.
