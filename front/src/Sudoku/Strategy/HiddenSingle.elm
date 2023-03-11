module Sudoku.Strategy.HiddenSingle exposing (strategy)

import Set exposing (Set)
import Sudoku exposing (Action, Problem, options)
import Sudoku.Blocks exposing (Block)
import Sudoku.Cell exposing (Cell)
import Sudoku.Domain exposing (Domain)
import Sudoku.Strategy exposing (Strategy)


strategy : Strategy
strategy problem =
    let
        hiddenSingles =
            Sudoku.options problem
                |> List.map Tuple.first
                |> List.concatMap (incidentBlocks problem)
                |> List.map (toHiddenOptions problem)
                |> List.filter (Tuple.second >> Set.size >> (==) 1)
                |> List.map toAction
    in
    if not <| List.isEmpty hiddenSingles then
        Just hiddenSingles

    else
        Nothing


incidentBlocks : Problem -> Cell -> List ( Cell, Block )
incidentBlocks problem cell =
    List.map (Tuple.pair cell) <| Sudoku.incidentBlocks problem cell


toHiddenOptions : Problem -> ( Cell, Block ) -> ( Cell, Set Domain )
toHiddenOptions problem ( cell, block ) =
    ( cell, hiddenOptions problem cell block )


hiddenOptions : Problem -> Cell -> Block -> Set Domain
hiddenOptions problem cell block =
    let
        candidates =
            Sudoku.candidatesAt cell problem

        others =
            Set.remove cell block
                |> Set.toList
                |> List.map (\c -> Sudoku.candidatesAt c problem)
                |> List.foldl Set.union Set.empty
    in
    Set.diff candidates others


toAction : ( Cell, Set Domain ) -> Action
toAction ( cell, options ) =
    let
        d =
            options
                |> Set.toList
                |> List.head
                -- Never occurs
                |> Maybe.withDefault -1
    in
    Sudoku.fill cell d
