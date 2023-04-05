module Visualizer.Tree exposing (Model, Msg, fromProblem, update, view)

import Html.Styled as Html exposing (Html)
import Html.Styled.Events as Event
import Set exposing (Set)
import Stack exposing (Stack)
import Sudoku exposing (Problem)
import Sudoku.Cell exposing (Cell)
import Sudoku.Domain exposing (Domain)


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
        , stack = Stack.empty |> Stack.push (frameFrom problem)
        , statistics = { nodesExplored = 1 }
        }


type alias Statistics =
    { nodesExplored : Int
    }


type alias Frame =
    { problem : Problem }


frameFrom : Problem -> Frame
frameFrom problem =
    { problem = problem }


type Msg
    = Explore Cell Domain


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        Explore cell d ->
            let
                frame =
                    Stack.peek model.stack

                newFrame =
                    model.stack
                        |> Stack.peek
                        |> Maybe.map .problem
                        |> Maybe.map (Sudoku.execute (Sudoku.fill cell d))
                        |> Maybe.map frameFrom
            in
            case newFrame of
                Just f ->
                    let
                        s =
                            model.statistics

                        statistics =
                            { s | nodesExplored = s.nodesExplored + 1 }
                    in
                    ( Model
                        { model
                            | stack = Stack.push f model.stack
                            , statistics = statistics
                        }
                    , Cmd.none
                    )

                Nothing ->
                    ( Model model, Cmd.none )


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
        [ Html.span [] [ Html.text <| "#nodes: " ++ String.fromInt statistics.nodesExplored ]
        ]


viewFrame : Frame -> Html Msg
viewFrame frame =
    let
        option =
            frame.problem
                |> Sudoku.options
                -- TODO include preference
                |> List.head

        content =
            case option of
                Nothing ->
                    viewNoOptions frame

                Just ( cell, ds ) ->
                    viewOptions cell ds
    in
    Html.div []
        [ content
        ]


viewNoOptions : Frame -> Html msg
viewNoOptions _ =
    Html.text "no options left"


viewOptions : Cell -> Set Domain -> Html Msg
viewOptions cell ds =
    let
        viewOption : Domain -> Html Msg
        viewOption d =
            Html.li []
                [ Html.a [ Event.onClick (Explore cell d) ]
                    [ Html.text <| String.fromInt d
                    ]
                ]
    in
    Html.div []
        [ Html.span [] [ Html.text <| "cell: " ++ String.fromInt cell ]
        , Html.ul [] <|
            List.map viewOption
                (Set.toList
                    --TODO order candidates
                    ds
                )
        ]
