module Sudoku.Execute exposing (Model, Msg, empty, update, view)

import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event
import Sudoku exposing (Action, Problem)
import Sudoku.Solver as Solver exposing (Plan)


type alias Model =
    { problem : Problem
    , plan : Maybe Plan
    , actions : List Action
    }


empty : Problem -> Model
empty problem =
    { problem = problem, plan = Nothing, actions = [] }


type Msg
    = Solve
    | Advance


update : Msg -> Model -> Model
update message model =
    case message of
        Solve ->
            let
                plan =
                    Solver.solve model.problem
            in
            { model | plan = plan }

        Advance ->
            let
                action =
                    model.plan
                        |> Maybe.andThen List.head
                        |> Maybe.map Tuple.first
            in
            action
                |> Maybe.map (\a -> { model | plan = model.plan |> Maybe.andThen List.tail, actions = a :: model.actions, problem = Sudoku.execute a model.problem })
                |> Maybe.withDefault model


type alias Info =
    { m : Int }


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
                Just plan ->
                    Debug.toString plan

                Nothing ->
                    "?"
    in
    Html.span [] [ Html.text content ]
