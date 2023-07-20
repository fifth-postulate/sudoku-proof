module HiddenSingleTest exposing (..)

import SolveTest exposing (solveSingleStepTest)
import Sudoku exposing (clue)
import Sudoku.Strategy.HiddenSingle as HiddenSingle
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
                            |> Maybe.andThen (clue 6 2)

                    expected =
                        Sudoku.emptySudoku 4
                            |> clue 0 1
                            |> Maybe.andThen (clue 6 2)
                            |> Maybe.andThen (clue 1 2)
                  in
                  solveSingleStepTest HiddenSingle.strategy "hidden single single step" problem expected
                ]
            ]
        ]
