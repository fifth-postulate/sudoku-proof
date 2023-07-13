module SimpleStrategyTest exposing (..)

import Expect
import Sudoku exposing (Problem, clue)
import Sudoku.Strategy as Strategy exposing (Plan, Strategy)
import Sudoku.Strategy.Combinator exposing (either, repeated)
import Sudoku.Strategy.HiddenSingle as HiddenSingle
import Sudoku.Strategy.None as NakesSingle
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
                            |> clue 7 1
                            |> Just
                  in
                  solveTest "forced" problem expected
                ]
            ]
        ]


strategy : Strategy
strategy =
    repeated <| either [ NakesSingle.strategy, HiddenSingle.strategy ]


solveTest : String -> Problem -> Maybe Problem -> Test
solveTest description problem expected =
    test description <|
        \_ ->
            let
                suggestion =
                    strategy problem

                actual =
                    suggestion
                        |> Maybe.map (\p -> Strategy.execute p problem)
            in
            Expect.equal expected actual
