module Visualizer.ExecuteLeastComplexPlan exposing (Model, Msg, empty, toProblem, update, view)

import Css exposing (..)
import Css.Global exposing (global, selector)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event
import Set
import Sudoku exposing (Action(..), Problem)
import Sudoku.Strategy exposing (Plan)
import Sudoku.Strategy.LeastComplexPlan as Solver


type Model
    = Unsolved Problem
    | Solved Problem Execution


type alias Execution =
    { future : Plan, history : Plan }


empty : Problem -> Model
empty problem =
    Unsolved problem


toProblem : Model -> Problem
toProblem model =
    case model of
        Unsolved problem ->
            problem

        Solved problem { history } ->
            history
                |> List.foldr Sudoku.execute problem


type Msg
    = Solve
    | Advance
    | Retreat


update : Msg -> Model -> Model
update message model =
    case ( message, model ) of
        ( Solve, Unsolved problem ) ->
            Solver.strategy (complexity problem) problem
                |> Maybe.map (\plan -> Solved problem { future = plan, history = [] })
                |> Maybe.withDefault model

        ( Advance, Solved problem execution ) ->
            Solved problem <| advance execution

        ( Retreat, Solved problem execution ) ->
            Solved problem <| retreat execution

        _ ->
            model


complexity : Problem -> Plan -> Int
complexity problem plan =
    case plan of
        [] ->
            1

        p :: ps ->
            case p of
                Fill c _ ->
                    let
                        cost =
                            Sudoku.candidatesAt c problem
                                |> Set.size
                    in
                    cost * complexity (Sudoku.execute p problem) ps


advance : Execution -> Execution
advance execution =
    execution.future
        |> List.head
        |> Maybe.map (\a -> { execution | future = execution.future |> List.drop 1, history = a :: execution.history })
        |> Maybe.withDefault execution


retreat : Execution -> Execution
retreat execution =
    execution.history
        |> List.head
        |> Maybe.map (\a -> { execution | future = a :: execution.future, history = List.drop 1 execution.history })
        |> Maybe.withDefault execution


type alias Info =
    { m : Int }


view : Info -> Model -> Html Msg
view info model =
    case model of
        Unsolved problem ->
            viewUnsolved info problem

        Solved problem execution ->
            viewSolved info problem execution


viewUnsolved : Info -> Problem -> Html Msg
viewUnsolved info problem =
    Html.div []
        [ Html.div [] [ Html.button [ Event.onClick Solve ] [ Html.text "🝢" ] ]
        , Sudoku.view info problem
        ]


viewSolved : Info -> Problem -> Execution -> Html Msg
viewSolved info problem execution =
    let
        current =
            execution.history
                |> List.foldr Sudoku.execute problem
    in
    Html.div []
        [ global
            [ selector "span.plan-step::before" [ property "content" "\"[\"" ]
            , selector "span.plan-step > span:nth-child(n+2)::before" [ property "content" "\",\"" ]
            , selector "span.plan-step::after" [ property "content" "\"]\"" ]
            ]
        , viewControls
        , viewFuture current execution.future
        , Sudoku.view info current
        , viewHistory execution.history
        ]


viewControls : Html Msg
viewControls =
    Html.div []
        [ Html.button [ Event.onClick Retreat ] [ Html.text "⊟" ]
        , Html.button [ Event.onClick Advance ] [ Html.text "⊞" ]
        ]


viewFuture : Problem -> Plan -> Html msg
viewFuture problem plan =
    let
        content =
            plan
                |> List.map viewStep
    in
    Html.div []
        [ viewComplexity problem plan
        , Html.span [ Attribute.classList [ ( "plan-step", True ) ] ] content
        ]


viewStep : Action -> Html msg
viewStep action =
    let
        ( cell, d ) =
            Sudoku.clueFrom action
    in
    Html.span []
        [ Html.span [] [ Html.text <| String.fromInt cell ]
        , Html.span [] [ Html.text "↦" ]
        , Html.span [] [ Html.text <| String.fromInt d ]
        ]


viewComplexity : Problem -> Plan -> Html msg
viewComplexity problem plan =
    Html.span [] [ Html.text <| String.fromInt <| complexity problem plan ]


viewHistory : Plan -> Html msg
viewHistory plan =
    plan
        |> List.reverse
        |> List.map viewAction
        |> Html.ol []


viewAction : Action -> Html msg
viewAction action =
    Html.li []
        [ Sudoku.viewAction action
        ]
