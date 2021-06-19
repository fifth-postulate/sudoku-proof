module SolverTest exposing (..)

import Expect
import Sudoku exposing (Problem, clue)
import Test exposing (..)


suite : Test
suite =
    describe "Sudoku"
        [ describe "solve"
            [ let
                problem =
                    Sudoku.emptySudoku 4
                        |> clue 0 1
                        |> clue 1 2
                        |> clue 2 3

                expected =
                    Sudoku.emptySudoku 4
                        |> clue 0 1
                        |> clue 1 2
                        |> clue 2 3
                        |> clue 3 4
                        |> Just
              in
              solveTest "forced" problem expected
            , let
                problem =
                    Sudoku.emptySudoku 4
                        |> clue 0 1
                        |> clue 6 4

                expected =
                    Sudoku.emptySudoku 4
                        |> clue 0 1
                        |> clue 6 4
                        |> clue 8 4
                        |> Just
              in
              solveTest "baby sudoku with singleRemaining" problem expected
            ]
        ]


solveTest : String -> Problem -> Maybe Problem -> Test
solveTest description problem expected =
    test description <|
        \_ ->
            let
                suggestion =
                    Sudoku.solve problem

                actual =
                    suggestion
                        |> Maybe.map (\action -> Sudoku.execute action problem)
            in
            Expect.equal expected actual
