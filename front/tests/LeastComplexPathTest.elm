module LeastComplexPathTest exposing (..)

import SolveTest exposing (solveSingleStepTest)
import Sudoku exposing (clue)
import Sudoku.Strategy exposing (Plan)
import Sudoku.Strategy.LeastComplexPlan as LeastComplexPath
import Test exposing (..)


suite : Test
suite =
    describe "Sudoku"
        [ describe "Stategy"
            [ describe "LeastComplexPath"
                [ let
                    problem =
                        Sudoku.emptySudoku 4
                            |> clue 0 1
                            |> Maybe.andThen (clue 1 2)
                            |> Maybe.andThen (clue 2 3)
                            |> Maybe.andThen (clue 3 4)
                            |> Maybe.andThen (clue 4 3)
                            |> Maybe.andThen (clue 5 4)
                            |> Maybe.andThen (clue 6 1)
                            |> Maybe.andThen (clue 7 2)
                            |> Maybe.andThen (clue 8 2)
                            |> Maybe.andThen (clue 9 3)
                            |> Maybe.andThen (clue 10 4)
                            |> Maybe.andThen (clue 11 1)
                            |> Maybe.andThen (clue 12 4)
                            |> Maybe.andThen (clue 13 1)
                            |> Maybe.andThen (clue 14 2)

                    expected =
                        Sudoku.emptySudoku 4
                            |> clue 0 1
                            |> Maybe.andThen (clue 1 2)
                            |> Maybe.andThen (clue 2 3)
                            |> Maybe.andThen (clue 3 4)
                            |> Maybe.andThen (clue 4 3)
                            |> Maybe.andThen (clue 5 4)
                            |> Maybe.andThen (clue 6 1)
                            |> Maybe.andThen (clue 7 2)
                            |> Maybe.andThen (clue 8 2)
                            |> Maybe.andThen (clue 9 3)
                            |> Maybe.andThen (clue 10 4)
                            |> Maybe.andThen (clue 11 1)
                            |> Maybe.andThen (clue 12 4)
                            |> Maybe.andThen (clue 13 1)
                            |> Maybe.andThen (clue 14 2)
                            |> Maybe.andThen (clue 15 3)
                  in
                  solveSingleStepTest (LeastComplexPath.strategy complexity) "least complex path single step" problem expected
                ]
            ]
        ]


complexity : Plan -> Int
complexity plan =
    List.length plan
