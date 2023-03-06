module NakedSingleTest exposing (..)

import Expect
import Sudoku exposing (Problem, clue)
import Sudoku.Strategy exposing (Plan)
import Sudoku.Strategy.NakedSingle exposing (strategy)
import Test exposing (..)


suite : Test
suite =
    describe "Sudoku"
        [ describe "Strategy"
            [ describe "NakedSingle"
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
               ]
            ]
        ]


solveTest : String -> Problem -> Maybe Problem -> Test
solveTest description problem expected =
    test description <|
        \_ ->
            let
                suggestion =
                    strategy problem

                actual =
                    suggestion
                        |> Maybe.andThen List.head
                        |> Maybe.map Tuple.first
                        |> Maybe.map (\action -> Sudoku.execute action problem)
            in
            Expect.equal expected actual
