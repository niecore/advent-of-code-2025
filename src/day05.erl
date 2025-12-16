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

part2() ->
    {FreshFoods, _} = parse_input(),
