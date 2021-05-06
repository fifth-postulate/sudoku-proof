module Solver.Kernel exposing (forced, singleRemaining)

import Set
import Set.Util exposing (pick)
import Sudoku.Kernel exposing (Rule, cellRule, shouldBe, tripleCellBlockRule)


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


singleRemaining : Rule
singleRemaining =
    let
        rule target first second =
            let
                remaining =
                    Set.diff target <| Set.union first second
            in
            if Set.size target == 3 && Set.size remaining == 1 then
                remaining
                    |> pick
                    |> Maybe.map shouldBe

            else
                Nothing
    in
    tripleCellBlockRule rule
