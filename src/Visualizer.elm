module Visualizer exposing (..)

import Browser
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event
import Sudoku exposing (Action, Info, Plan, Problem, clue, emptySudoku)


main =
    let
        info =
            { m = 4 }
    in
    Browser.sandbox
        { init = init info
        , update = update
        , view = view info >> Html.toUnstyled
        }


type alias Model =
    { problem : Problem
    , plan : Maybe Plan
    , actions : List Action
    }


init : Info -> Model
init info =
    let
        problem =
            emptySudoku info.m
                |> clue 0 2
                |> clue 5 3
                |> clue 11 2
                |> clue 14 4
    in
    { problem = problem, plan = Nothing, actions = [] }


type Msg
    = Solve
    | Advance


update : Msg -> Model -> Model
update msg model =
    case msg of
        Solve ->
            let
                plan =
                    Sudoku.solve model.problem
            in
            { model | plan = plan }

        Advance ->
            let
                action =
                    model.plan
                        |> Maybe.andThen List.head
            in
            action
                |> Maybe.map (\a -> { model | plan = model.plan |> Maybe.andThen List.tail, actions = a :: model.actions })
                |> Maybe.withDefault model


view : Info -> Model -> Html Msg
view info model =
    Html.div []
        [ Html.button [ Event.onClick Solve, Attribute.disabled <| hasPlan model ] [ Html.text "🝢" ]
        , Html.button [ Event.onClick Advance, Attribute.disabled <| not <| hasPlan model ] [ Html.text "🍍" ]
        , Html.div []
            [ viewPlan model.plan
            , Sudoku.view info model.problem
            , viewActions model.actions
            ]
        ]


hasPlan : Model -> Bool
hasPlan { plan } =
    case plan of
        Just _ ->
            True

        Nothing ->
            False


viewActions : List Action -> Html msg
viewActions actions =
    Html.ol [ Attribute.reversed True ] <| List.map viewAction actions


viewAction : Action -> Html msg
viewAction action =
    Html.li []
        [ Sudoku.viewAction action
        ]

viewPlan : Maybe Plan -> Html msg
viewPlan option =
    let
        content =
            case option of
               Just plan -> Debug.toString plan

               Nothing -> "?"
    in
    Html.span [] [Html.text content]
