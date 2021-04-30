module Sudoku.Blocks exposing (sudokuBlocks)

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
sudokuSquares _ =
    -- TODO
    []
