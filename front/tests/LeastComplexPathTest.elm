module LeastComplexPathTest exposing (..)

import Expect
import Sudoku exposing (Problem, clue)
import Sudoku.Strategy exposing (Plan)
import Sudoku.Strategy.LeastComplexPlan exposing (strategy)
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
                            |> clue 1 2
                            |> clue 2 3
                            |> clue 3 4
                            |> clue 4 3
                            |> clue 5 4
                            |> clue 6 1
                            |> clue 7 2
                            |> clue 8 2
                            |> clue 9 3
                            |> clue 10 4
                            |> clue 11 1
                            |> clue 12 4
                            |> clue 13 1
                            |> clue 14 2

                    expected =
                        Sudoku.emptySudoku 4
                            |> clue 0 1
                            |> clue 1 2
                            |> clue 2 3
                            |> clue 3 4
                            |> clue 4 3
                            |> clue 5 4
                            |> clue 6 1
                            |> clue 7 2
                            |> clue 8 2
                            |> clue 9 3
                            |> clue 10 4
                            |> clue 11 1
                            |> clue 12 4
                            |> clue 13 1
                            |> clue 14 2
                            |> clue 15 3
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
                complexity : Plan -> Int
                complexity plan =
                    plan
                        |> List.map Tuple.second
                        |> List.foldl (*) 1

                suggestion =
                    strategy complexity problem

                actual =
                    suggestion
                        |> Maybe.andThen List.head
                        |> Maybe.map Tuple.first
                        |> Maybe.map (\action -> Sudoku.execute action problem)
            in
            Expect.equal expected actual
