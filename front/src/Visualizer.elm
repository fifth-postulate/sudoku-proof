module Visualizer exposing (..)

import Browser
import Html.Styled as Html exposing (Html)
import Html.Styled.Events as Event
import Sudoku exposing (clue, emptySudoku)
import Visualizer.Entry as Entry
import Visualizer.ExecuteLeastComplexPlan as Execute


main : Program {} Model Msg
main =
    Browser.element
        { init = \_ -> init 4
        , update = update
        , view = view >> Html.toUnstyled
        , subscriptions = subscriptions
        }


type Model
    = Prepare Entry.Model
    | Play Info Execute.Model


type alias Info =
    { m : Int }


init : Int -> ( Model, Cmd Msg )
init _ =
    let
        m =
            9

        problem =
            emptySudoku m
                |> clue 79 7
                |> clue 75 8
                |> clue 72 1
                |> clue 69 2
                |> clue 66 5
                |> clue 62 9
                |> clue 59 4
                |> clue 57 2
                |> clue 54 6
                |> clue 53 7
                |> clue 47 8
                |> clue 43 2
                |> clue 41 3
                |> clue 39 6
                |> clue 37 9
                |> clue 33 9
                |> clue 27 2
                |> clue 26 6
                |> clue 23 1
                |> clue 21 4
                |> clue 18 3
                |> clue 14 5
                |> clue 11 6
                |> clue 8 3
                |> clue 5 9
                |> clue 1 5
    in
    ( Prepare <| Entry.fromProblem m problem, Cmd.none )


type Msg
    = PrepareMsg Entry.Msg
    | PlayMsg Execute.Msg
    | GoPlay
    | Stop


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case ( message, model ) of
        ( PrepareMsg msg, Prepare mdl ) ->
            let
                ( m, cmd ) =
                    Entry.update msg mdl
            in
            ( Prepare m, Cmd.map PrepareMsg cmd )

        ( GoPlay, Prepare mdl ) ->
            let
                info =
                    { m = mdl.m }

                m =
                    mdl
                        |> Entry.toProblem
                        |> Execute.empty
            in
            ( Play info m, Cmd.none )

        ( PlayMsg msg, Play info mdl ) ->
            let
                m =
                    Execute.update msg mdl
            in
            ( Play info m, Cmd.none )

        ( Stop, Play info mdl ) ->
            let
                problem =
                    Execute.toProblem mdl
            in
            ( Prepare <| Entry.fromProblem info.m problem, Cmd.none )

        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ viewControl model
        , viewContent model
        ]


viewControl : Model -> Html Msg
viewControl model =
    let
        content =
            case model of
                Prepare _ ->
                    [ Html.button [ Event.onClick GoPlay ] [ Html.text "▶️" ] ]

                Play _ _ ->
                    [ Html.button [ Event.onClick Stop ] [ Html.text "⏹️" ] ]
    in
    Html.div [] content


viewContent : Model -> Html Msg
viewContent model =
    case model of
        Prepare mdl ->
            Html.map PrepareMsg <| Entry.view mdl

        Play info mdl ->
            Html.map PlayMsg <| Execute.view info mdl


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
