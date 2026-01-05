-module(math_utils).
-export([mod/2, max_with_index/1, gcd/2, lcm/2]).

mod(X, Y) when Y > 0 ->
    case X rem Y of
        R when R < 0 -> R + Y;
        R -> R
    end;
mod(X, Y) when Y < 0 ->
    case X rem Y of
        R when R > 0 -> R + Y;
        R -> R
    end.

min(X, Y) when X < Y -> X;
min(_, Y) -> Y.

max(X, Y) when X > Y -> X;
max(_, Y) -> Y.

max_with_index([H | T]) ->
    %% 1 is the max indice
    %% 2 is the next indice
    %% H is the current max value
    max_with_index(T, H, 1, 2).
max_with_index([], MaxVal, MaxIdx, _Idx) ->
    %% base case
    {MaxVal, MaxIdx};
max_with_index([H | T], MaxVal, MaxIdx, Idx) when H > MaxVal ->
    %% Found new max value set
    %% new max value and indice
    max_with_index(T, H, Idx, Idx + 1);
max_with_index([_ | T], MaxVal, MaxIdx, Idx) ->
    %% current head is smaller than max value
    max_with_index(T, MaxVal, MaxIdx, Idx + 1).

gcd(A, 0) -> abs(A);
gcd(A, B) -> gcd(B, A rem B).

lcm(A, B) -> abs(A * B) div gcd(A, B).
