-module(day10part2).
-export([test/0, part2/1]).

-import(linalg, [shape/1, cell/3, zeros/2, set_cell/4, row/2, col/2, set_row/3]).

-record(machine, {light, buttons, joltages}).

test() ->
    Machine = #machine{light= [], buttons = [[3], [1,3], [2], [2,3], [0,2], [0,1]], joltages = [3, 5, 4, 7]},
    Matrix = create_augmented_matrix(Machine),
    %% Expected: 4x7 matrix
    %% [0, 0, 0, 0, 1, 1, 3]
    %% [0, 1, 0, 0, 0, 1, 5]
    %% [0, 0, 1, 1, 1, 0, 4]
    %% [1, 1, 0, 1, 0, 0, 7]
    {4, 7} = shape(Matrix),
    3 = cell(1, 7, Matrix),
    5 = cell(2, 7, Matrix),
    ok.

%% Reference: https://www.reddit.com/r/adventofcode/comments/1pp98cr/2025_day_10_part_2_solution_without_using_a_3rd
part2(Machines) ->
    lists:map(fun(Machine) ->
        Matrix = create_augmented_matrix(Machine),
        io:format("Augmented Matrix: ~p~n", [Matrix]),
        {M, FreeVars} = row_echelon(Matrix),
        io:format("Row echelon Matrix: ~p~n", [M]),
        io:format("Free variables (columns): ~p~n", [FreeVars]),

        MaxPresses = [max_presses_for_button(FreeVar, Machine) || FreeVar <- FreeVars],
        io:format("Max presses for each button: ~p ~n", [MaxPresses]),

        Solution = back_substitution(M),
        io:format("Solution: ~p~n", [Solution]),
        lists:sum(Solution)
    end, Machines).

max_presses_for_button(FreeVar, #machine{buttons = Buttons, joltages = Joltages}) ->
    Button = lists:nth(FreeVar, Buttons),
    AffectedJoltages = [lists:nth(Idx + 1, Joltages) || Idx <- Button],
    MaxPresses = lists:min(AffectedJoltages),
    MaxPresses.

create_augmented_matrix(#machine{buttons = Buttons, joltages = Joltages}) ->
    NumRows = length(Joltages),
    NumCols = length(Buttons) + 1,
    %% Create empty matrix filled with zeros
    Matrix = zeros(NumRows, NumCols),
    IndexedButtons = lists:enumerate(Buttons),
    %% Fill in the matrix values
    lists:foldl(
        fun({RowIdx, Joltage}, Acc) ->
            %% Set button columns
            Acc1 = lists:foldl(
                fun({ColIdx, Button}, M) ->
                    case lists:member(RowIdx - 1, Button) of
                        true  -> set_cell(RowIdx, ColIdx, 1, M);
                        false -> M
                    end
                end,
                Acc,
                IndexedButtons
            ),
            %% Set joltage column (last column)
            set_cell(RowIdx, NumCols, Joltage, Acc1)
        end,
        Matrix,
        lists:enumerate(Joltages)
    ).

find_pivot(Matrix, Col) ->
    {_Above, Below} = lists:split(Col - 1, Matrix),
    CurrentCol = col(Col, Below),
    find_nonzero(lists:enumerate(CurrentCol)).

find_nonzero([{Idx, Val} | _]) when Val =/= 0 ->
    {Idx, Val};
find_nonzero([_ | Rest]) ->
    find_nonzero(Rest);
find_nonzero([]) ->
    no_pivot.

row_echelon(Matrix) ->
    {NumRows, NumCols} = shape(Matrix),
    %% Don't process the augmented column (last column)
    MaxCol = min(NumRows, NumCols - 1),
    row_echelon(Matrix, 1, MaxCol, []).

row_echelon(Matrix, Col, MaxCol, FreeVars) when Col > MaxCol ->
    {Matrix, lists:reverse(FreeVars)};
row_echelon(Matrix, Col, MaxCol, FreeVars) ->
    io:format("Col ~p Matrix: ~p~n", [Col, Matrix]),

    case find_pivot(Matrix, Col) of
        {PivotIdx, PivotValue} ->
            io:format("Pivot ~p Value: ~p~n", [PivotIdx, PivotValue]),
            %% Swap pivot row with current row
            Matrix1 = swap_rows(Matrix, PivotIdx + Col - 1, Col),
            %% Ensure pivot is positive by scaling row if needed
            Matrix2 = ensure_positive_pivot(Matrix1, Col, PivotValue),
            Matrix3 = eliminate_column(Matrix2, Col, abs(PivotValue)),
            row_echelon(Matrix3, Col + 1, MaxCol, FreeVars);
        no_pivot ->
            io:format("No pivot found in column ~p (free variable)~n", [Col]),
            %% Track free variable and continue to next column
            row_echelon(Matrix, Col + 1, MaxCol, [Col | FreeVars])
    end.

ensure_positive_pivot(Matrix, RowIdx, PivotValue) when PivotValue < 0 ->
    io:format("Scale row ~p by ~p~n", [RowIdx, -1]),
    scale_row(Matrix, RowIdx, -1);
ensure_positive_pivot(Matrix, _RowIdx, _PivotValue) ->
    Matrix.

eliminate_column(Matrix, Col, PivotValue) ->
    {_Above, Below} = lists:split(Col - 1, Matrix),
    CurrentCol = col(Col, Below),
    NonZeroRows = [{Idx + Col, Val} || {Idx, Val} <- lists:enumerate(tl(CurrentCol)), Val =/= 0],
    lists:foldl(
        fun({RowIdx, RowValue}, M) ->
            Lcm = math_utils:lcm(PivotValue, RowValue),
            M1 = scale_row(M, Col, Lcm),
            M2 = scale_row(M1, RowIdx, Lcm),
            io:format("Scale row ~p by ~p~n", [Col, Lcm]),
            io:format("Scale row ~p by ~p~n", [RowIdx, Lcm]),

            case RowValue > 0 of
                true ->
                    io:format("Subtract row ~p from row ~p~n", [Col, RowIdx]),
                    subtract_rows(M2, RowIdx, Col);
                false ->
                    io:format("Add row ~p to row ~p~n", [Col, RowIdx]),
                    add_rows(M2, RowIdx, Col)
            end
        end,
        Matrix,
        NonZeroRows
    ).

back_substitution(Matrix) ->
    back_substitution(lists:reverse(Matrix), 1, length(Matrix), []).

back_substitution(_Matrix, RowIdx, N, Solutions) when RowIdx > N ->
    Solutions;
back_substitution(Matrix, RowIdx, N, Solutions) ->
    Row = row(RowIdx, Matrix),
    System = lists:nthtail(N - RowIdx, Row),
    Coeffs = lists:sublist(System, 2, length(System) - 2),
    B = lists:last(System),
    Result = lists:foldl(fun({Coeff, X}, Acc) -> Acc - (Coeff * X) end, B, lists:zip(Coeffs, Solutions)),
    Coeff = hd(System),
    X = Result / Coeff,
    back_substitution(Matrix, RowIdx + 1, N, [X | Solutions]).

swap_rows(Matrix, Idx, Idx) ->
    Matrix;
swap_rows(Matrix, Row1Idx, Row2Idx) ->
    Row1 = row(Row1Idx, Matrix),
    Row2 = row(Row2Idx, Matrix),
    Matrix2 = set_row(Row1Idx, Row2, Matrix),
    set_row(Row2Idx, Row1, Matrix2).

scale_row(Matrix, RowIdx, 1) ->
    Matrix;
scale_row(Matrix, RowIdx, Scalar) ->
    Row = row(RowIdx, Matrix),
    NewRow = lists:map(fun(X) -> X * Scalar end, Row),
    set_row(RowIdx, NewRow, Matrix).

subtract_rows(Matrix, Row1Idx, Row2Idx) ->
    Row1 = row(Row1Idx, Matrix),
    Row2 = row(Row2Idx, Matrix),
    NewRow = lists:map(fun({X1, X2}) -> X1 - X2 end, lists:zip(Row1, Row2)),
    set_row(Row1Idx, NewRow, Matrix).

add_rows(Matrix, Row1Idx, Row2Idx) ->
    Row1 = row(Row1Idx, Matrix),
    Row2 = row(Row2Idx, Matrix),
    NewRow = lists:map(fun({X1, X2}) -> X1 + X2 end, lists:zip(Row1, Row2)),
    set_row(Row1Idx, NewRow, Matrix).
