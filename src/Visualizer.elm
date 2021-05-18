module Visualizer exposing (..)

import Browser
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event
import Sudoku exposing (Info, Msg(..), Problem, clue, emptySudoku)


main =
    let
        info =
            { m = 9 }
    in
    Browser.sandbox
        { init = init info
        , update = Sudoku.update
        , view = view info >> Html.toUnstyled
        }


init : Info -> Problem
init info =
    emptySudoku info.m
        |> clue 2 9
        |> clue 4 5
        |> clue 9 5
        |> clue 10 3
        |> clue 13 8
        |> clue 14 4
        |> clue 16 2
        |> clue 21 6
        |> clue 26 4
        |> clue 27 4
        |> clue 29 6
        |> clue 31 3
        |> clue 37 1
        |> clue 38 8
        |> clue 44 9
        |> clue 51 2
        |> clue 61 5
        |> clue 64 8
        |> clue 69 7
        |> clue 75 5
        |> clue 76 6
        |> clue 77 1
        |> clue 78 9


view : Info -> Problem -> Html Sudoku.Msg
view info problem =
    Html.div []
        [ Html.button [ Event.onClick Advance, Attribute.disabled <| Sudoku.isSolved problem ] [ Html.text "↻" ]
        , Sudoku.view info problem
        ]
