module Sudoku.Solver exposing (Plan, Strategy)

import Sudoku exposing (Action, Problem)


type alias Strategy =
    Problem -> Maybe Plan


type alias Plan =
    List ( Action, Int )