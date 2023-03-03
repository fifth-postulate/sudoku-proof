module Sudoku.Blocks exposing (Block, sudokuBlocks)

import List.Util exposing (cartesianProduct)
import Set exposing (Set)
import Sudoku.Cell exposing (Cell)


type alias Block =
    Set Cell


sudokuBlocks : Int -> List Block
sudokuBlocks m =
    sudokuRows m ++ sudokuColumns m ++ sudokuSquares m


sudokuRows : Int -> List Block
sudokuRows m =
    List.range 0 (m - 1)
        |> List.map (sudokuRow m)


sudokuRow : Int -> Int -> Block
sudokuRow m index =
    let
        row =
            m * index
    in
    List.range 0 (m - 1)
        |> List.map ((+) row)
        |> Set.fromList


sudokuColumns : Int -> List Block
sudokuColumns m =
    List.range 0 (m - 1)
        |> List.map (sudokuColumn m)


sudokuColumn : Int -> Int -> Block
sudokuColumn m index =
    List.range 0 (m - 1)
        |> List.map ((*) m)
        |> List.map ((+) index)
        |> Set.fromList


sudokuSquares : Int -> List Block
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


square : Int -> Block
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
