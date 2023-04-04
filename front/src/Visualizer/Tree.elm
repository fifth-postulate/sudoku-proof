module Visualizer.Tree exposing (Model)

import Html.Styled as Html exposing (Html)
import Stack exposing (Stack)
import Sudoku exposing (Problem)


type Model
    = Model
        { info : Sudoku.Info
        , stack : Stack Frame
        , statistics : Statistics
        }


fromProblem : Sudoku.Info -> Problem -> Model
fromProblem info problem =
    Model
        { info = info
        , stack = Stack.empty |> Stack.push (frame problem)
        , statistics = { nodesExplored = 1 }
        }


type alias Statistics =
    { nodesExplored : Int
    }


type alias Frame =
    { problem : Problem }


frame : Problem -> Frame
frame problem =
    { problem = problem }


type Msg
    = DoNothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view (Model model) =
    let
        problem =
            model.stack
                |> Stack.peek
                |> Maybe.map .problem
                |> Maybe.withDefault (Sudoku.emptySudoku model.info.m)
    in
    Html.div []
        [ Html.div []
            [ viewStatistics model.statistics
            , Stack.view viewFrame model.stack
            ]
        , Sudoku.view model.info problem
        ]


viewStatistics : Statistics -> Html Msg
viewStatistics statistics =
    Html.div []
        [ Html.span [] [ Html.text "#nodes: " ++ String.fromInt statistics.nodesExplored ]
        ]


viewFrame : Frame -> Html Msg
viewFrame frame =
    Html.div []
        [ Htmt.text "exploring frame"
        ]
