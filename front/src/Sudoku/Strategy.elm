module Sudoku.Strategy exposing (Plan, Strategy, execute)

import Sudoku exposing (Action, Problem)


type alias Strategy =
    Problem -> Maybe Plan


type alias Plan =
    List Action


execute : Plan -> Problem -> Problem
execute plan problem =
    plan
        |> List.foldl Sudoku.execute problem
