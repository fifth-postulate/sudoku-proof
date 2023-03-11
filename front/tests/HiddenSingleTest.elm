module HiddenSingleTest exposing (..)

import Expect
import Sudoku exposing (Problem, clue)
import Sudoku.Strategy exposing (Plan)
import Sudoku.Strategy.HiddenSingle exposing (strategy)
import Test exposing (..)


suite : Test
suite =
    describe "Sudoku"
        [ describe "Strategy"
            [ describe "HiddenSingle"
                [ let
                    problem =
                        Sudoku.emptySudoku 4
                            |> clue 0 1
                            |> clue 6 2

                    expected =
                        Sudoku.emptySudoku 4
                            |> clue 0 1
                            |> clue 6 2
                            |> clue 1 2
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
                        |> Maybe.map (\action -> Sudoku.execute action problem)
            in
            Expect.equal expected actual
