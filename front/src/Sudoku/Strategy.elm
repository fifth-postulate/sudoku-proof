module Sudoku.Strategy exposing (Plan, Strategy, execute)

import Sudoku exposing (Action, Problem)


type alias Strategy =
    Problem -> Maybe Plan


type alias Plan =
    List Action


execute : Plan -> Problem -> Maybe Problem
execute plan problem =
    plan
        |> List.foldl (lift Sudoku.execute) (Just problem)


lift : (Action -> Problem -> Maybe Problem) -> Action -> Maybe Problem -> Maybe Problem
lift f a p =
    Maybe.andThen (f a) p
