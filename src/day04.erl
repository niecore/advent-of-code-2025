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
    PaperRolls = maps:filter(fun(Pos, Val) -> Val =:= 1 end, RoomMap),
    Neighbors = [{-1, -1}, {-1, 0}, {-1, 1}, {0, -1}, {0, 1}, {1, -1}, {1, 0}, {1, 1}],
    NeighborsMap = maps:map(fun({X, Y}, _) -> [maps:get({X + DX, Y + DY}, RoomMap, 0) || {DX, DY} <- Neighbors] end, PaperRolls),
    NeighborsMapCount = maps:map(fun(_, Neighbors) -> lists:sum(Neighbors) end, NeighborsMap),
    MovablePaperRolls = maps:filter(fun(_, Count) -> Count < 4 end, NeighborsMapCount).

remove_paper_rolls(RoomMap, Moved) ->
    MovablePaperRolls = movable_paper_rolls(RoomMap),
    MovablePaperRollsCount = maps:size(MovablePaperRolls),
    case MovablePaperRollsCount of
        0 -> Moved;
        _ ->
            NewPaperRolls = maps:merge(RoomMap, maps:map(fun(_, _) -> 0 end, MovablePaperRolls)),
            remove_paper_rolls(NewPaperRolls, Moved + MovablePaperRollsCount)
    end.
