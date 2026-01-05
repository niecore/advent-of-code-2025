-module(day10part2).
-export([test/0, part2/1]).

-import(linalg, [shape/1, cell/3, zeros/2, set_cell/4, row/2, col/2, set_row/3]).


-record(machine, {light, buttons, joltages}).

test() ->
    Machine = #machine{light= [], buttons = [[3], [1,3], [2], [2,3], [0,2], [0,1]], joltages = [3, 5, 4, 7]},
    Matrix = create_augmented_matrix_from_machine(Machine),
    %% Expected: 4x7 matrix
    %% [0, 0, 0, 0, 1, 1, 3]
    %% [0, 1, 0, 0, 0, 1, 5]
    %% [0, 0, 1, 1, 1, 0, 4]
    %% [1, 1, 0, 1, 0, 0, 7]
    {4, 7} = shape(Matrix),
    3 = cell(1, 7, Matrix),
    5 = cell(2, 7, Matrix),
    ok.

%% had to look up a solution on this one:
%% https://www.reddit.com/r/adventofcode/comments/1pp98cr/2025_day_10_part_2_solution_without_using_a_3rd
part2(Machines) ->
    io:format("Machines: ~p~n", [hd(Machines)]),
    Matrix = create_augmented_matrix_from_machine(hd(Machines)),
    io:format("Matrix: ~p~n", [Matrix]),
    eliminate(Matrix).

create_augmented_matrix_from_machine(Machine) ->
    NumRows = length(Machine#machine.joltages),
    NumCols = length(Machine#machine.buttons) + 1,

    %% Create empty matrix filled with zeros
    Matrix = zeros(NumRows, NumCols),

    %% Fill in the matrix values
    lists:foldl(fun({RowIdx, Joltage}, Acc) ->
        %% Set button columns
        Acc2 = lists:foldl(fun({ColIdx, Button}, M) ->
            case lists:member(RowIdx - 1, Button) of
                true -> set_cell(RowIdx, ColIdx, 1, M);
                false -> M
            end
        end, Acc, lists:enumerate(Machine#machine.buttons)),
        %% Set joltage column (last column)
        set_cell(RowIdx, NumCols, Joltage, Acc2)
    end, Matrix, lists:enumerate(Machine#machine.joltages)).

find_pivot(Matrix, Col, N) ->
    {Above, Below} = lists:split(Col - 1, Matrix),
    CurrentCol = col(Col, Below),

    case lists:search(fun({PivotIdx, PivotValue}) -> PivotValue =/= 0 end, lists:enumerate(CurrentCol)) of
        {value, Value} -> Value;
        _ -> error
    end.

eliminate(Matrix) ->
    N = length(Matrix),
    RowEchelonForMatrix = eliminate(Matrix, 1, N),
    RowEchelonForMatrix.

eliminate(Matrix, Col, N) when Col >= N ->
    Matrix;

eliminate(Matrix, Col, N) ->
    {PivotIdx, PivotValue} = find_pivot(Matrix, Col, N),
    io:format("Pivot Value: ~p, Pivot Index: ~p~n", [PivotValue, PivotIdx]),

    %% Swap pivot row with current row
    Matrix2 = swap_rows(Matrix, PivotIdx + (Col - 1), Col),

    %% Ensure pivot is positive by scaling row if needed
    ScaleFactor = case PivotValue < 0 of
        true -> -1;
        false -> 1
    end,
    Matrix3 = scale_row(Matrix2, Col, ScaleFactor),
    ActualPivotValue = abs(PivotValue),

    {Above2, Below2} = lists:split(Col - 1, Matrix3),
    CurrentCol2 = col(Col, Below2),

    NonZeroRows = [ {RowIdx + Col, Value} || {RowIdx, Value} <- lists:enumerate(tl(CurrentCol2)), Value =/= 0 ],
    Matrix4 = lists:foldl(fun({RowIdx, RowValue}, M) ->
        Lcm = math_utils:lcm(ActualPivotValue, RowValue),
        M2 = scale_row(M, Col, Lcm),
        M3 = scale_row(M2, RowIdx, Lcm),
        M4 = subtract_rows(M3, RowIdx, Col),
        M4
    end, Matrix3, NonZeroRows),

    io:format("Matrix after elimination: ~p~n", [Matrix4]),
    eliminate(Matrix4, Col + 1, N).

swap_rows(Matrix, Row1Idx, Row2Idx) ->
    Row1 = row(Row1Idx, Matrix),
    Row2 = row(Row2Idx, Matrix),
    Matrix2 = set_row(Row1Idx, Row2, Matrix),
    set_row(Row2Idx, Row1, Matrix2).

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
