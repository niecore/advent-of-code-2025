-module(list_utils).
-export([cartesian/2, cartesian_n/1]).

cartesian(L1, L2) -> [{X, Y} || X <- L1, Y <- L2].

cartesian_n([]) -> [[]];
cartesian_n([H|T]) -> [[X|Y] || X <- H, Y <- cartesian_n(T)].
