module NakedSingleTest exposing (..)

import SolveTest exposing (solveSingleStepTest)
import Sudoku exposing (clue)
import Sudoku.Strategy.NakedSingle as NakedSingle
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
                            |> Maybe.andThen (clue 1 2)
                            |> Maybe.andThen (clue 2 3)

                    expected =
                        Sudoku.emptySudoku 4
                            |> clue 0 1
                            |> Maybe.andThen (clue 1 2)
                            |> Maybe.andThen (clue 2 3)
                            |> Maybe.andThen (clue 3 4)
                  in
                  solveSingleStepTest NakedSingle.strategy "naked single single step" problem expected
                ]
            ]
        ]
