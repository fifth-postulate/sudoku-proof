module Sudoku.Clue exposing (Clue)

import Sudoku.Cell exposing (Cell)
import Sudoku.Domain exposing (Domain)


type alias Clue =
    ( Cell, Domain )
