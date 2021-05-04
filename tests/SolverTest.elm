module SolverTest exposing (..)

import Expect
import Solver.Kernel as Solver
import Sudoku.Kernel as Sudoku exposing (Strategy, hint)
import Test exposing (..)


strategy : Strategy
strategy =
    [ Solver.forced ]


suite : Test
suite =
    describe "Solver"
        [ describe "forced"
            [ test "baby sudoku with first three columns suggest the foruth column" <|
                \_ ->
                    let
                        problem =
                            Sudoku.emptySudoku 4
                                |> hint 0 1
                                |> hint 1 2
                                |> hint 2 3

                        suggestion =
                            Sudoku.suggest strategy problem

                        actual =
                            suggestion
                                |> Maybe.map (\action -> Sudoku.execute action problem)

                        expected =
                            Sudoku.emptySudoku 4
                                |> hint 0 1
                                |> hint 1 2
                                |> hint 2 3
                                |> hint 3 4
                                |> Just
                    in
                    Expect.equal expected actual
            ]
        ]
