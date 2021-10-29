module Visualizer exposing (..)

import Browser
import Html.Styled as Html exposing (Html)
import Html.Styled.Events as Event
import Sudoku.Entry as Entry
import Sudoku.Execute as Execute


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
init m =
    ( Prepare <| Entry.empty m, Cmd.none )


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
