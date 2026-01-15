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
    [0, 1, 0, 0, 10] = free_var_row(4, 2, 10),
    ok.

%% Reference: https://www.reddit.com/r/adventofcode/comments/1pp98cr/2025_day_10_part_2_solution_without_using_a_3rd
part2(Machines) ->
    MinSolutions = lists:map(fun(Machine) ->
        %% First we transform the buttons and joltage requirements into a augmented matrix
        %% which represents the system of linear equations that we need to solve to find a valid solution
        Matrix = create_augmented_matrix(Machine),
        {RowsN, ColumnsN} = shape(Matrix),

        %% Perform gaussian elimination to solve to bring the matrix into row echelon form
        {M1, FreeVars} = row_echelon(Matrix),
        M2 = lists:filter(fun(Row) -> not lists:all(fun(X) -> X == 0 end, Row) end, M1), %% filter 0s

        %% Calculate for free the variables all possible combinations that would
        %% not overshoot the jolatage contraints
        MaxPressesContraints = [max_presses_for_button(FreeVar, Machine) || FreeVar <- FreeVars],
        FreeVarCombinations = list_utils:cartesian_n([lists:seq(0,MaxPress) || MaxPress <- MaxPresses]),

        Solutions = lists:map(
            fun(FreeVarGuesses) ->
                %% For every free variable we insert a row in the matrix where the free
                %% variable is set to a guessed value. I.e. free var "4" guessed value "10" adds:
                %% 0 0 0 1 0 0 10
                M2 = lists:foldl(
                    fun({FreeVar, GuessedValue}, M3) ->
                        Row = free_var_row(ColumnsN - 1, FreeVar, GuessedValue),
                        insert_row(M4, FreeVar, Row)
                    end,
                    M2,
                    lists:keysort(1, lists:zip(FreeVars, FreeVarGuesses))
                ),

                %% The back substitution algorithm was not able to handle matrices
                %% that are not in exact row echelon form. Probably there is another fix
                %% to either detect if the solution is invalid but i just rerun the row_echolon
                %% algorithm again.
                {M4, _FreeVars} = row_echelon(M3),
                M5 = lists:filter(fun(Row) -> not lists:all(fun(X) -> X == 0 end, Row) end, M4), %% filter 0s

                %% Check if we have any rows where all coeffs are zero but the rhs has a value
                case lists:any(fun is_inconsistent_row/1, M5) of
                    true ->
                        %% return an invalid solution
                        [-1];
                    false ->
                        back_substitution(M5)
                end
            end,
            FreeVarCombinations
        ),

        %% Filter our solutions to not have negative button pressed and also no
        %% "half" presses - means only whole numbers.
        IsGreaterZero = fun(N) -> N >= 0 end,
        IsWholeNumber = fun(N) -> N == trunc(N) end,
        ValidSolutions = lists:filter(
            fun(Solution) ->
                lists:all(IsGreaterZero, Solution) andalso
                lists:all(IsWholeNumber, Solution)
            end,
            Solutions
        ),

        %% lists:sum is the button presses per valid solution
        %% lists:min is the shortest path of button presses
        Result = lists:min([lists:sum(Solution) || Solution <- ValidSolutions]),
        Result
    end, Machines),

    %% calculate the button presses of all machines
    lists:sum(MinSolutions).

is_inconsistent_row(Row) ->
    Coeffs = lists:droplast(Row),
    B = lists:last(Row),
    lists:all(fun(X) -> X == 0 end, Coeffs) andalso B =/= 0.

free_var_row(Length, Col, Value) ->
    System = lists:map(
        fun(Idx) ->
            case Idx of
                Col -> 1;
                _ -> 0
            end
        end,
        lists:seq(1, Length)
    ),
    System ++ [Value].

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

%%%
%%% Row Echelon Form Functions
%%%
row_echelon(Matrix) ->
    {NumRows, NumCols} = shape(Matrix),
    %% Don't process the augmented column (last column)
    MaxCol = min(NumRows, NumCols - 1),
    row_echelon(Matrix, 1, MaxCol, []).

row_echelon(Matrix, Col, MaxCol, FreeVars) when Col > MaxCol ->
    %% If there are leftover columns we list them here as free variables.
    AdditionalFreeVars = lists:seq(Col, length(hd(Matrix)) - 1),
    {Matrix, lists:reverse(AdditionalFreeVars ++ FreeVars)};
row_echelon(Matrix, Col, MaxCol, FreeVars) ->
    case find_pivot(Matrix, Col) of
        {PivotIdx, PivotValue} ->
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

ensure_positive_pivot(Matrix, RowIdx, PivotValue) when PivotValue < 0 ->
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
            PivotScale = Lcm div abs(PivotValue),
            TargetScale = Lcm div abs(RowValue),
            M1 = scale_row(M, Col, PivotScale),
            M2 = scale_row(M1, RowIdx, TargetScale),
            M3 = case RowValue > 0 of
                true -> subtract_rows(M2, RowIdx, Col);
                false -> add_rows(M2, RowIdx, Col)
            end,
            %% Scale pivot row back to
            div_row(M3, Col, PivotScale)
        end,
        Matrix,
        NonZeroRows
    ).

%%%
%%% Back Substition Functions
%%%
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


%%%
%%% Matrix helper functions
%%%
swap_rows(Matrix, Idx, Idx) ->
    Matrix;
swap_rows(Matrix, Row1Idx, Row2Idx) ->
    %%io:format("Swap rows ~p and ~p~n", [Row1Idx, Row2Idx]),
    Row1 = row(Row1Idx, Matrix),
    Row2 = row(Row2Idx, Matrix),
    Matrix2 = set_row(Row1Idx, Row2, Matrix),
    set_row(Row2Idx, Row1, Matrix2).

scale_row(Matrix, RowIdx, 1) ->
    Matrix;
scale_row(Matrix, RowIdx, Scalar) ->
    %%io:format("Scale row ~p by ~p~n", [RowIdx, Scalar]),
    Row = row(RowIdx, Matrix),
    NewRow = lists:map(fun(X) -> X * Scalar end, Row),
    set_row(RowIdx, NewRow, Matrix).

subtract_rows(Matrix, Row1Idx, Row2Idx) ->
    %%io:format("Subtract row ~p from row ~p~n", [Row2Idx, Row1Idx]),
    Row1 = row(Row1Idx, Matrix),
    Row2 = row(Row2Idx, Matrix),
    NewRow = lists:map(fun({X1, X2}) -> X1 - X2 end, lists:zip(Row1, Row2)),
    set_row(Row1Idx, NewRow, Matrix).

add_rows(Matrix, Row1Idx, Row2Idx) ->
    %% io:format("Add row ~p to row ~p~n", [Row2Idx, Row1Idx]),
    Row1 = row(Row1Idx, Matrix),
    Row2 = row(Row2Idx, Matrix),
    NewRow = lists:map(fun({X1, X2}) -> X1 + X2 end, lists:zip(Row1, Row2)),
    set_row(Row1Idx, NewRow, Matrix).

div_row(Matrix, Row1Idx, Divisor) ->
    %%io:format("Divide row ~p by ~p~n", [Row1Idx, Divisor]),
    Row = row(Row1Idx, Matrix),
    NewRow = [X div Divisor || X <- Row],
    set_row(Row1Idx, NewRow, Matrix).

insert_row(Matrix, RowIdx, Row) when RowIdx >= length(Matrix) ->
    Matrix ++ [Row];
insert_row(Matrix, RowIdx, Row) ->
    %%io:format("Insert row ~p at index ~p~n", [Row, RowIdx]),
    {Head, Tail} = lists:split(RowIdx - 1, Matrix),
    Head ++ [Row | Tail].
