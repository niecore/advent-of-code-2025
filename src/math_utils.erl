-module(math_utils).
-export([mod/2]).

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
