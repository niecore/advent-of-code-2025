-module(day04).
-export([test/0, part1/0, part2/0]).

test() ->
    ok.

part1() ->
    Room = input:read_input(4),
    RoomList = [{{X, Y}, case Val of $@ -> 1; _ -> 0 end} ||{X, Line} <- lists:enumerate(Room), {Y, Val} <- lists:enumerate(Line)],
    RoomMap = maps:from_list(RoomList),
    MovablePaperRolls = movable_paper_rolls(RoomMap),
    maps:size(MovablePaperRolls).

part2() ->
    Room = input:read_input(4),
    RoomList = [{{X, Y}, case Val of $@ -> 1; _ -> 0 end} ||{X, Line} <- lists:enumerate(Room), {Y, Val} <- lists:enumerate(Line)],
    RoomMap = maps:from_list(RoomList),
    remove_paper_rolls(RoomMap, 0).

movable_paper_rolls(RoomMap) ->
    maps:filter(fun(Pos, 1) -> count_neighbors(Pos, RoomMap) < 4;
                    (_, _) -> false
                end, RoomMap).

count_neighbors({X, Y}, RoomMap) ->
    lists:sum([maps:get({X + DX, Y + DY}, RoomMap, 0)
                || DX <- [-1, 0, 1], DY <- [-1, 0, 1], {DX, DY} =/= {0, 0}]).

remove_paper_rolls(RoomMap, Moved) ->
    MovablePaperRolls = movable_paper_rolls(RoomMap),
    MovablePaperRollsCount = maps:size(MovablePaperRolls),
    case MovablePaperRollsCount of
        0 -> Moved;
        _ ->
            NewPaperRolls = maps:merge(RoomMap, maps:map(fun(_, _) -> 0 end, MovablePaperRolls)),
            remove_paper_rolls(NewPaperRolls, Moved + MovablePaperRollsCount)
    end.
