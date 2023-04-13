module Visualizer.Tree exposing (Model, Msg, fromProblem, toProblem, update, view)

import Css exposing (..)
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event
import Set exposing (Set)
import Stack exposing (Stack)
import Sudoku exposing (Problem)
import Sudoku.Cell exposing (Cell)
import Sudoku.Domain exposing (Domain)
import Sudoku.Strategy as Strategy exposing (Plan, Strategy)


type Model
    = Model
        { info : Sudoku.Info
        , stack : Stack Frame
        , statistics : Statistics
        , cheap : Strategy
        }


fromProblem : Strategy -> Sudoku.Info -> Problem -> Model
fromProblem cheap info problem =
    Model
        { info = info
        , stack = Stack.empty |> Stack.push (frameFrom cheap problem)
        , statistics = { nodesExplored = 1, maximumDepth = 1 }
        , cheap = cheap
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
    { problem : Problem
    , progress : Progress
    }


type Progress
    = Cheap Plan
    | Guess Plan
    | Unexplored


frameFrom : Strategy -> Problem -> Frame
frameFrom cheap problem =
    let
        progress =
            cheap problem
                |> Maybe.map Cheap
                |> Maybe.withDefault Unexplored
    in
    { problem = problem
    , progress = progress
    }


type Msg
    = Explore Cell Domain
    | Execute Plan
    | DropFrame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        Explore cell d ->
            let
                newFrame =
                    model.stack
                        |> Stack.peek
                        |> Maybe.map .problem
                        |> Maybe.map (Sudoku.execute (Sudoku.fill cell d))
                        |> Maybe.map (frameFrom model.cheap)
            in
            case newFrame of
                Just f ->
                    ( Model
                        { model
                            | stack = Stack.push f model.stack
                            , statistics = updateStatistics <| Model model
                        }
                    , Cmd.none
                    )

                Nothing ->
                    ( Model model, Cmd.none )

        Execute plan ->
            let
                newFrame =
                    model.stack
                        |> Stack.peek
                        |> Maybe.map .problem
                        |> Maybe.map (Strategy.execute plan)
                        |> Maybe.map (frameFrom model.cheap)
            in
            case newFrame of
                Just f ->
                    ( Model
                        { model
                            | stack = Stack.push f model.stack
                            , statistics = updateStatistics <| Model model
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


updateStatistics : Model -> Statistics
updateStatistics (Model model) =
    let
        s =
            model.statistics

        currentDepth =
            Stack.depth model.stack

        maximumDepth =
            max currentDepth s.maximumDepth
    in
    { s | nodesExplored = s.nodesExplored + 1, maximumDepth = maximumDepth }


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
                |> List.sortBy (Tuple.second >> Set.size)
                |> List.head

        content =
            case option of
                Nothing ->
                    viewNoOptions frame

                Just ( cell, ds ) ->
                    if index == 0 then
                        wrap <| viewOptions frame.problem cell ds

                    else
                        viewOptions frame.problem cell ds

        shortcut =
            case frame.progress of
                Cheap plan ->
                    Html.span [ Event.onClick <| Execute plan ] [ Html.text "↑" ]

                _ ->
                    Html.span [] []
    in
    Html.div []
        [ shortcut
        , content
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


viewOptions : Problem -> Cell -> Set Domain -> Html Msg
viewOptions problem cell ds =
    let
        actualFrequency =
            Sudoku.frequency problem

        frequency d =
            actualFrequency
                |> Dict.get d
                |> Maybe.withDefault 0

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
                (ds
                    |> Set.toList
                    |> List.sortBy frequency
                )
        ]
