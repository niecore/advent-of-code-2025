-module(day12).
-export([test/0, part1/0, part2/0]).

-record(machine, {light, buttons, joltages}).

test() ->
    9 = max_area_of_present(["###","##.","##."]),
    7 = min_area_of_present(["###","##.","##."]),
    ok.

parse_input() ->
    Input = input:read_input_string(12),
    Groups = string:split(Input, "\n\n", all),
    GroupLines = [string:split(G, "\n", all) || G <- Groups],

    Presents = lists:sublist(GroupLines, length(GroupLines) - 1),
    PresentMap = maps:from_list(lists:map(fun([KeyLine | Pattern]) ->
        Key = list_to_integer(string:trim(KeyLine, trailing, ":")),
        {Key, Pattern}
    end, Presents)),

    Grids = lists:last(GroupLines),
    ParsedGrids = lists:map(fun(Line) ->
        [Dims, PresentsStr] = string:split(Line, ": "),
        [W, H] = string:split(Dims, "x"),
        Width = list_to_integer(W),
        Height = list_to_integer(H),
        PresentsList = lists:map(fun list_to_integer/1, string:split(PresentsStr, " ", all)),
        {Width, Height, PresentsList}
    end, Grids),

    {PresentMap, ParsedGrids}.

part1() ->
    {Presents, Grids} = parse_input(),
    length([Grid || Grid <- Grids, required_present_area_in_grid(Grid, Presents) < area_of_grid(Grid)]).

max_area_of_present(Present) ->
    length(Present) * length(hd(Present)).

min_area_of_present(Present) ->
    lists:sum(lists:map(fun(Line) -> length([C || C <- Line, C =:= $#]) end, Present)).

area_of_grid({Width, Height, _}) ->
    Width * Height.

required_present_area_in_grid({_Width, _Height, PresentsList}, Presents) ->
    lists:sum([min_area_of_present(maps:get(PresentId - 1, Presents)) * PresentCount || {PresentId, PresentCount} <- lists:enumerate(PresentsList)]).

part2() ->
    Input = parse_input().
