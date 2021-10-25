module Sudoku.Execute exposing (Model, Msg, empty, toProblem, update, view)

import Css exposing (..)
import Css.Global exposing (global, selector)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event
import Sudoku exposing (Action, Problem)
import Sudoku.Solver as Solver exposing (Plan)


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
                |> List.map Tuple.first
                |> List.foldr Sudoku.execute problem


type Msg
    = Solve
    | Advance
    | Retreat


update : Msg -> Model -> Model
update message model =
    case ( message, model ) of
        ( Solve, Unsolved problem ) ->
            Solver.solve problem
                |> Maybe.map (\plan -> Solved problem { future = plan, history = [] })
                |> Maybe.withDefault model

        ( Advance, Solved problem execution ) ->
            Solved problem <| advance execution

        ( Retreat, Solved problem execution ) ->
            Solved problem <| retreat execution

        _ ->
            model


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
                |> List.map Tuple.first
                |> List.foldr Sudoku.execute problem
    in
    Html.div []
        [ global
            [ selector "span.plan-step::before" [ property "content" "\"[\"" ]
            , selector "span.plan-step > span:nth-child(n+2)::before" [ property "content" "\",\"" ]
            , selector "span.plan-step::after" [ property "content" "\"]\"" ]
            ]
        , viewControls execution
        , viewFuture execution.future
        , Sudoku.view info current
        , viewHistory execution.history
        ]


viewControls : Execution -> Html Msg
viewControls { future, history } =
    Html.div []
        [ Html.button [ Event.onClick Retreat ] [ Html.text "⊟" ]
        , Html.button [ Event.onClick Advance ] [ Html.text "⊞" ]
        ]


viewFuture : Plan -> Html msg
viewFuture plan =
    let
        content =
            plan
                |> List.map viewStep
    in
    Html.div []
        [ viewComplexity plan
        , Html.span [ Attribute.classList [ ( "plan-step", True ) ] ] content
        ]


viewStep : ( Action, Int ) -> Html msg
viewStep ( action, c ) =
    let
        ( cell, d ) =
            Sudoku.toClue action
    in
    Html.span []
        [ Html.span [] [ Html.text <| String.fromInt cell ]
        , Html.span [] [ Html.text "↦" ]
        , Html.sub [] [ Html.text <| String.fromInt c ]
        , Html.span [] [ Html.text <| String.fromInt d ]
        ]


viewComplexity : Plan -> Html msg
viewComplexity plan =
    Html.span [] [ Html.text <| String.fromInt <| Solver.complexity plan ]


viewHistory : Plan -> Html msg
viewHistory plan =
    plan
        |> List.map Tuple.first
        |> List.map viewAction
        |> Html.ol [ Attribute.reversed True ]


viewAction : Action -> Html msg
viewAction action =
    Html.li []
        [ Sudoku.viewAction action
        ]
