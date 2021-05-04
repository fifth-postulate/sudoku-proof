module Solver.Kernel exposing (..)

import Set
import Set.Util exposing (pick)
import Sudoku.Kernel exposing (Rule, cellRule, shouldBe)


forced : Rule
forced =
    let
        singleCandidate candidates =
            if Set.size candidates == 1 then
                candidates
                    |> pick
                    |> Maybe.map shouldBe

            else
                Nothing
    in
    cellRule singleCandidate
