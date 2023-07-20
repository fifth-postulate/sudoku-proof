module SimpleStrategyTest exposing (..)

import SolveTest exposing (solveSingleStepTest, solveTest)
import Sudoku exposing (clue)
import Sudoku.Strategy.Combinator exposing (either, repeated)
import Sudoku.Strategy.HiddenSingle as HiddenSingle
import Sudoku.Strategy.NakedSingle as NakedSingle
import Test exposing (..)


suite : Test
suite =
    describe "Sudoku"
        [ describe "Strategy"
            [ describe "[NakedSingle, HiddenSingle]"
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

                    strategy =
                        either [ NakedSingle.strategy, HiddenSingle.strategy ]
                  in
                  solveSingleStepTest strategy "[N, H] single step" problem expected
                ]
            , describe "repeated [NakedSingle, HiddenSingle]"
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
                            |> Maybe.andThen (clue 7 1)

                    strategy =
                        repeated <| either [ NakedSingle.strategy, HiddenSingle.strategy ]
                  in
                  solveTest strategy "r [N, H] single step" problem expected
                ]
            ]
        ]
