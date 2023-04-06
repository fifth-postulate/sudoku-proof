module Visualizer.Tree exposing (Model, Msg, fromProblem, toProblem, update, view)

import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
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
        , statistics = { nodesExplored = 1, maximumDepth = 1 }
        }


toProblem : Model -> Problem
toProblem (Model model) =
    model.stack
        |> Stack.peek
        |> Maybe.map .problem
        |> Maybe.withDefault (Sudoku.emptySudoku model.info.m)


type alias Statistics =
    { nodesExplored : Int
    , maximumDepth : Int
    }


type alias Frame =
    { problem : Problem }


frameFrom : Problem -> Frame
frameFrom problem =
    { problem = problem }


type Msg
    = Explore Cell Domain
    | DropFrame


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

                        currentDepth =
                            Stack.depth model.stack

                        maximumDepth =
                            max currentDepth s.maximumDepth

                        statistics =
                            { s | nodesExplored = s.nodesExplored + 1, maximumDepth = maximumDepth }
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

        DropFrame ->
            let
                stack =
                    model.stack
                        |> Stack.pop
                        |> Tuple.second
            in
            ( Model { model | stack = stack }, Cmd.none )


view : Model -> Html Msg
view (Model model) =
    let
        problem =
            model.stack
                |> Stack.peek
                |> Maybe.map .problem
                |> Maybe.withDefault (Sudoku.emptySudoku model.info.m)
    in
    Html.div
        [ Attribute.css
            [ displayFlex
            ]
        ]
        [ Html.div [ Attribute.css [ width <| px 250 ] ]
            [ viewStatistics model.statistics
            , Stack.view viewFrame model.stack
            ]
        , Sudoku.view model.info problem
        ]


viewStatistics : Statistics -> Html Msg
viewStatistics statistics =
    Html.div []
        [ Html.span [] [ Html.text <| "#nodes: " ++ String.fromInt statistics.nodesExplored ]
        , Html.span [] [ Html.text <| "max depth: " ++ String.fromInt statistics.maximumDepth ]
        ]


viewFrame : Int -> Frame -> Html Msg
viewFrame index frame =
    let
        option =
            frame.problem
                |> Sudoku.options
                -- TODO include cell preference
                |> List.head

        content =
            case option of
                Nothing ->
                    viewNoOptions frame

                Just ( cell, ds ) ->
                    if index == 0 then
                        wrap <| viewOptions cell ds

                    else
                        viewOptions cell ds
    in
    Html.div []
        [ content
        ]


viewNoOptions : Frame -> Html msg
viewNoOptions frame =
    if Sudoku.isSolved frame.problem then
        Html.text "Solved"

    else
        Html.text "Stuck"


wrap : Html Msg -> Html Msg
wrap content =
    Html.div [ Attribute.css [ position relative ] ]
        [ content
        , Html.span
            [ Attribute.css
                [ right <| px 3
                , top <| px 3
                , position absolute
                ]
            , Event.onClick DropFrame
            ]
            [ Html.text "❌" ]
        ]


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
    Html.div [ Attribute.css [ position relative ] ]
        [ Html.span [] [ Html.text <| "cell: " ++ String.fromInt cell ]
        , Html.ul [] <|
            List.map viewOption
                (Set.toList
                    --TODO order candidates
                    ds
                )
        ]
