module Sudoku.Strategy.NakedSingle exposing (strategy)

import Set exposing (Set)
import Sudoku
import Sudoku.Cell exposing (Cell)
import Sudoku.Domain exposing (Domain)
import Sudoku.Strategy exposing (Plan, Strategy)


strategy : Strategy
strategy problem =
    let
        isNakedSingle options =
            1 == Set.size options

        nakedSingles =
            problem
                |> Sudoku.options
                |> List.filter (Tuple.second >> isNakedSingle)
                |> List.concatMap toPlan
    in
    if not <| List.isEmpty nakedSingles then
        Just nakedSingles

    else
        Nothing


toPlan : ( Cell, Set Domain ) -> Plan
toPlan ( cell, options ) =
    let
        toAction option =
            Sudoku.fill cell option
    in
    options
        |> Set.toList
        |> List.map toAction
