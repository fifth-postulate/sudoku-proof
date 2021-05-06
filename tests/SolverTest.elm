module SolverTest exposing (..)

import Expect
import Solver.Kernel as Solver
import Sudoku.Kernel as Sudoku exposing (Problem, Strategy, hint)
import Test exposing (..)


suite : Test
suite =
    describe "Solver"
        [ describe "forced"
            [ let
                problem =
                    Sudoku.emptySudoku 4
                        |> hint 0 1
                        |> hint 1 2
                        |> hint 2 3

                expected =
                    Sudoku.emptySudoku 4
                        |> hint 0 1
                        |> hint 1 2
                        |> hint 2 3
                        |> hint 3 4
                        |> Just
              in
              strategyTest "baby sudoku with forced rule" problem expected
            -- , let
            --     problem =
            --         Sudoku.emptySudoku 4
            --             |> hint 0 1
            --             |> hint 6 4

            --     expected =
            --         Sudoku.emptySudoku 4
            --             |> hint 0 1
            --             |> hint 6 4
            --             |> hint 1 4
            --             |> Just
            --   in
            --   strategyTest "baby sudoku with singleRemaining" problem expected
            ]
        ]


strategy : Strategy
strategy =
    [ Solver.forced
    , Solver.singleRemaining
    ]


strategyTest : String -> Problem -> Maybe Problem -> Test
strategyTest description problem expected =
    test description <|
        \_ ->
            let
                suggestion =
                    Sudoku.suggest strategy problem

                actual =
                    suggestion
                        |> Maybe.map (\action -> Sudoku.execute action problem)
            in
            Expect.equal expected actual
