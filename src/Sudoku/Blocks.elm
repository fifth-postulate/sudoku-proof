module Sudoku.Blocks exposing (square, sudokuBlocks, sudokuSquares)

import List.Util exposing (cartesianProduct)
import Set exposing (Set)


type alias Cell =
    Int


sudokuBlocks : Int -> List (Set Cell)
sudokuBlocks m =
    sudokuRows m ++ sudokuColumns m ++ sudokuSquares m


sudokuRows : Int -> List (Set Cell)
sudokuRows m =
    List.range 0 (m - 1)
        |> List.map (sudokuRow m)


sudokuRow : Int -> Int -> Set Cell
sudokuRow m index =
    let
        row =
            m * index
    in
    List.range 0 (m - 1)
        |> List.map ((+) row)
        |> Set.fromList


sudokuColumns : Int -> List (Set Cell)
sudokuColumns m =
    List.range 0 (m - 1)
        |> List.map (sudokuColumn m)


sudokuColumn : Int -> Int -> Set Cell
sudokuColumn m index =
    List.range 0 (m - 1)
        |> List.map ((*) m)
        |> List.map ((+) index)
        |> Set.fromList


sudokuSquares : Int -> List (Set Cell)
sudokuSquares n =
    let
        m =
            n |> toFloat |> sqrt |> floor

        base =
            square m

        block ( row, column ) =
            Set.map (\v -> v + n * m * row + column * m) base

        indices =
            List.range 0 (m - 1)
    in
    cartesianProduct indices indices
        |> List.map block


square : Int -> Set Cell
square m =
    let
        n =
            m * m

        row =
            List.range 0 (m - 1)

        addConstant constant cells =
            List.map (\v -> v + constant) cells
    in
    List.range 0 (m - 1)
        |> List.concatMap (\rowIndex -> addConstant (rowIndex * n) row)
        |> Set.fromList
