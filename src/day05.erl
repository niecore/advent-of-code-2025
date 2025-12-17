-module(day05).
-export([test/0, part1/0, part2/0]).

test() ->
    ok.

parse_input() ->
    Input = input:read_input(5),
    FreshFoodsStrings = lists:takewhile(fun(Line) -> string:trim(Line) =/= "" end, Input),
    FreshFoods = lists:map(fun(RangeStr) ->
        [Start, End] = string:split(RangeStr, "-"),
        {list_to_integer(Start), list_to_integer(End)}
    end, FreshFoodsStrings),
    FoodInventoryStrings = tl(lists:dropwhile(fun(Line) -> string:trim(Line) =/= "" end, Input)),
    FoodInventory = [list_to_integer(X) || X <- FoodInventoryStrings],
    {FreshFoods, FoodInventory}.

part1() ->
    {FreshFoods, FoodInventory} = parse_input(),

    length(lists:filter(fun(Food) ->
        lists:any(fun({Start, End}) -> Food >= Start andalso Food =< End end, FreshFoods)
    end, FoodInventory)).

part2_does_not_compute() ->
    %% Bruteforcing all ranges into a set.
    %% Does not compute in time.
    {FreshFoods, _} = parse_input(),
    Set = set:from_list([Id || {Start, End} <- FreshFoods, Id <- lists:seq(Start, End)]),
    length(set:to_list(Set)).

part2() ->
    {FreshFoods, _} = parse_input(),
    SortByStartIdx = fun({Start1, _}, {Start2, _}) -> Start1 < Start2 end,
    FreshFoodsSorted = lists:sort(SortByStartIdx, FreshFoods),
    {FreshFoodIds, _} = lists:foldl(fun
        ({Start, End}, {FoodCount, Idx}) when End < Idx ->
            %% Fully included range
            %% Skip the range when it ends before the current index
            {FoodCount, Idx};
        ({Start, End}, {FoodCount, Idx}) when Start < Idx ->
            %% Overlapping ranges
            %% When range starts before current inde
            %% calculate from index instead of start
            {FoodCount + End - Idx + 1, End + 1};
        ({Start, End}, {FoodCount, Idx}) ->
            %% New Range outside of previous
            %% Range starts after current index
            {FoodCount + End - Start + 1, End + 1}
    end, {0, 0}, FreshFoodsSorted),

    FreshFoodIds.
