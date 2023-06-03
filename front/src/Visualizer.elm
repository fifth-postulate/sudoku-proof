module Visualizer exposing (..)

import Browser
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event
import Sudoku exposing (Problem, clue, emptySudoku)
import Sudoku.Strategy.Combinator exposing (either, repeated)
import Sudoku.Strategy.HiddenSingle as HiddenSingle
import Sudoku.Strategy.NakedSingle as NakedSingle
import Task
import Visualizer.Entry as Entry
import Visualizer.ExecuteLeastComplexPlan as Execute
import Visualizer.RandomExplore as RandomExplore
import Visualizer.Tree as Tree


main : Program {} Model Msg
main =
    Browser.element
        { init = \_ -> init 4
        , update = update
        , view = view >> Html.toUnstyled
        , subscriptions = subscriptions
        }


type Model
    = Prepare StrategyPicked Entry.Model
    | PlayLeastComplexPath StrategyPicked Info Execute.Model
    | PlayTreePath StrategyPicked Info Tree.Model
    | PlayRandomExplore StrategyPicked Info RandomExplore.Model


type StrategyPicked
    = LeastComplexPath
    | Tree
    | RandomExplore


type alias Info =
    { m : Int }


init : Int -> ( Model, Cmd Msg )
init _ =
    let
        m =
            9

        problem =
            emptySudoku m
                |> clue 5 4
                |> Maybe.andThen (clue 7 2)
                |> Maybe.andThen (clue 8 8)
                |> Maybe.andThen (clue 9 4)
                |> Maybe.andThen (clue 11 6)
                |> Maybe.andThen (clue 17 5)
                |> Maybe.andThen (clue 18 1)
                |> Maybe.andThen (clue 22 3)
                |> Maybe.andThen (clue 24 6)
                |> Maybe.andThen (clue 30 3)
                |> Maybe.andThen (clue 32 1)
                |> Maybe.andThen (clue 37 8)
                |> Maybe.andThen (clue 38 7)
                |> Maybe.andThen (clue 42 1)
                |> Maybe.andThen (clue 43 4)
                |> Maybe.andThen (clue 48 7)
                |> Maybe.andThen (clue 50 9)
                |> Maybe.andThen (clue 56 2)
                |> Maybe.andThen (clue 58 1)
                |> Maybe.andThen (clue 62 3)
                |> Maybe.andThen (clue 63 9)
                |> Maybe.andThen (clue 69 5)
                |> Maybe.andThen (clue 71 7)
                |> Maybe.andThen (clue 72 6)
                |> Maybe.andThen (clue 73 7)
                |> Maybe.andThen (clue 75 4)
                |> Maybe.withDefault (emptySudoku m)
    in
    ( Prepare RandomExplore <| Entry.fromProblem m problem, Task.perform identity <| Task.succeed GoPlay )



--Task.perform identity <| Task.succeed GoPlay )


type Msg
    = PrepareMsg Entry.Msg
    | LeastComplexPathMsg Execute.Msg
    | TreeMsg Tree.Msg
    | RandomExploreMsg RandomExplore.Msg
    | PickStrategy StrategyPicked
    | GoPlay
    | Stop
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case ( message, model ) of
        ( PrepareMsg msg, Prepare s mdl ) ->
            let
                ( m, cmd ) =
                    Entry.update msg mdl
            in
            ( Prepare s m, Cmd.map PrepareMsg cmd )

        ( PickStrategy t, Prepare _ mdl ) ->
            ( Prepare t mdl, Cmd.none )

        ( GoPlay, Prepare s mdl ) ->
            let
                info =
                    { m = mdl.m }
            in
            case s of
                LeastComplexPath ->
                    let
                        m =
                            mdl
                                |> Entry.toProblem
                                |> Execute.empty
                    in
                    ( PlayLeastComplexPath s info m, Cmd.none )

                Tree ->
                    let
                        cheap =
                            repeated <| either [ NakedSingle.strategy, HiddenSingle.strategy ]

                        m =
                            mdl
                                |> Entry.toProblem
                                |> Tree.fromProblem cheap info
                    in
                    ( PlayTreePath s info m, Cmd.none )

                RandomExplore ->
                    let
                        m =
                            mdl
                                |> Entry.toProblem
                                |> RandomExplore.fromProblem info
                    in
                    ( PlayRandomExplore s info m, Cmd.none )

        ( LeastComplexPathMsg msg, PlayLeastComplexPath s info mdl ) ->
            let
                m =
                    Execute.update msg mdl
            in
            ( PlayLeastComplexPath s info m, Cmd.none )

        ( TreeMsg msg, PlayTreePath s info mdl ) ->
            let
                ( m, cmd ) =
                    Tree.update msg mdl
            in
            ( PlayTreePath s info m, Cmd.map TreeMsg cmd )

        ( Stop, PlayLeastComplexPath s info mdl ) ->
            let
                problem =
                    Execute.toProblem mdl
            in
            ( Prepare s <| Entry.fromProblem info.m problem, Cmd.none )

        ( Stop, PlayTreePath s info mdl ) ->
            let
                problem =
                    Tree.toProblem mdl
            in
            ( Prepare s <| Entry.fromProblem info.m problem, Cmd.none )

        ( Stop, PlayRandomExplore s info m ) ->
            let
                problem =
                    RandomExplore.toProblem m
            in
            ( Prepare s <| Entry.fromProblem info.m problem, Cmd.none )

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
                Prepare s _ ->
                    [ Html.input
                        [ Attribute.id "strategy:leastcomplexpath"
                        , Attribute.type_ "radio"
                        , Attribute.checked <| s == LeastComplexPath
                        , Event.onCheck <| \_ -> PickStrategy LeastComplexPath
                        ]
                        []
                    , Html.label [ Attribute.for "strategy:leastcomplexpath" ] [ Html.text "L" ]
                    , Html.input
                        [ Attribute.id "strategy:tree"
                        , Attribute.type_ "radio"
                        , Attribute.checked <| s == Tree
                        , Event.onCheck <| \_ -> PickStrategy Tree
                        ]
                        []
                    , Html.label [ Attribute.for "strategy:tree" ] [ Html.text "T" ]
                    , Html.input
                        [ Attribute.id "strategy:randomexplore"
                        , Attribute.type_ "radio"
                        , Attribute.checked <| s == RandomExplore
                        , Event.onCheck <| \_ -> PickStrategy RandomExplore
                        ]
                        []
                    , Html.label [ Attribute.for "strategy:randomexplore" ] [ Html.text "R" ]
                    , Html.button [ Event.onClick GoPlay ] [ Html.text "▶️" ]
                    ]

                _ ->
                    [ Html.button [ Event.onClick Stop ] [ Html.text "⏹️" ] ]
    in
    Html.div [] content


viewContent : Model -> Html Msg
viewContent model =
    case model of
        Prepare _ mdl ->
            Html.map PrepareMsg <| Entry.view mdl

        PlayLeastComplexPath _ info mdl ->
            Html.map LeastComplexPathMsg <| Execute.view info mdl

        PlayTreePath _ info mdl ->
            Html.map TreeMsg <| Tree.view mdl

        PlayRandomExplore _ info mdl ->
            Html.map RandomExploreMsg <| RandomExplore.view mdl


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
