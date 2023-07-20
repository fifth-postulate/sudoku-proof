module SolveTest exposing (solveSingleStepTest, solveTest)

import Expect
import Sudoku exposing (Problem)
import Sudoku.Strategy exposing (Strategy)
import Test exposing (..)


solveSingleStepTest : Strategy -> String -> Maybe Problem -> Maybe Problem -> Test
solveSingleStepTest strategy description problem expected =
    test description <|
        \_ ->
            let
                suggestion =
                    Maybe.andThen strategy problem

                actual =
                    suggestion
                        |> Maybe.andThen List.head
                        |> Maybe.andThen (\action -> Maybe.andThen (Sudoku.execute action) problem)
            in
            Expect.equal expected actual


solveTest : Strategy -> String -> Maybe Problem -> Maybe Problem -> Test
solveTest strategy description problem expected =
    test description <|
        \_ ->
            let
                suggestion =
                    Maybe.andThen strategy problem

                executeAllSuggestions actions =
                    List.foldl (\action p -> Maybe.andThen (Sudoku.execute action) p) problem actions

                actual =
                    suggestion
                        |> Maybe.andThen executeAllSuggestions
            in
            Expect.equal expected actual
