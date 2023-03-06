module Sudoku.Strategy exposing (Plan, Strategy, execute)

import Sudoku exposing (Action, Problem)


type alias Strategy =
    Problem -> Maybe Plan



-- abstract to plan with info


type alias Plan =
    List ( Action, Int )


execute : Plan -> Problem -> Problem
execute plan problem =
    plan
        |> List.map Tuple.first
        |> List.foldl Sudoku.execute problem
