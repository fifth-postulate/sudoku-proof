module Visualizer.RandomExplore exposing (Model, Msg, fromProblem, toProblem, update, view)

import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Event
import Sudoku exposing (Problem)
import Sudoku.Strategy.Combinator exposing (either, repeated)
import Sudoku.Strategy.HiddenSingle as HiddenSingle
import Sudoku.Strategy.NakedSingle as NakedSingle
import Sudoku.Strategy.None as None
import Task
import Visualizer.RandomExplore.Statistics as Statistics exposing (Statistics)
import Visualizer.RandomExplore.Suite as Suite exposing (Model, Msg(..), create)


type Model
    = Model
        { info : Sudoku.Info
        , problem : Problem
        , shortcut : ShortCut
        , statistics : Statistics
        , suite : Maybe Suite.Model
        }


type ShortCut
    = None
    | Cheap


fromProblem : Sudoku.Info -> Problem -> Model
fromProblem info problem =
    Model
        { info = info
        , problem = problem
        , shortcut = Cheap
        , statistics = Statistics.empty
        , suite = Nothing
        }


toProblem : Model -> Problem
toProblem (Model m) =
    m.problem


type Msg
    = Run Int
    | SuiteMsg Suite.Msg
    | Tally
    | Take ShortCut


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        Run numberOfRuns ->
            let
                strategy =
                    case model.shortcut of
                        None ->
                            None.strategy

                        Cheap ->
                            repeated (either [ NakedSingle.strategy, HiddenSingle.strategy ])
            in
            ( Model { model | suite = Just <| create strategy numberOfRuns model.problem }, Task.perform SuiteMsg <| Task.succeed Start )

        SuiteMsg m ->
            case model.suite of
                Just suite ->
                    let
                        ( s, c ) =
                            Suite.update m suite

                        cmd =
                            Cmd.map SuiteMsg c

                        cmds =
                            if Suite.finished suite then
                                [ cmd, Task.perform identity <| Task.succeed Tally ]

                            else
                                [ cmd ]
                    in
                    ( Model { model | suite = Just s }, Cmd.batch cmds )

                Nothing ->
                    ( Model model, Cmd.none )

        Tally ->
            let
                statistics =
                    model.suite
                        |> Maybe.map (Suite.register model.statistics)
                        |> Maybe.withDefault model.statistics
            in
            ( Model { model | statistics = statistics, suite = Nothing }, Cmd.none )

        Take shortcut ->
            ( Model { model | shortcut = shortcut }, Cmd.none )


view : Model -> Html Msg
view ((Model m) as model) =
    Html.div []
        [ viewControl model
        , Statistics.view m.statistics
        ]


viewControl : Model -> Html Msg
viewControl ((Model m) as model) =
    let
        run =
            runOf model
    in
    Html.div []
        [ Html.div []
            [ Html.input
                [ Attribute.type_ "radio"
                , Attribute.name "shortcut"
                , Attribute.id "none"
                , Attribute.value "none"
                , Attribute.checked (m.shortcut == None)
                , Event.onCheck (\_ -> Take None)
                ]
                []
            , Html.label [ Attribute.for "none" ] [ Html.text "None" ]
            , Html.input
                [ Attribute.type_ "radio"
                , Attribute.name "shortcut"
                , Attribute.id "cheap"
                , Attribute.value "cheap"
                , Attribute.checked (m.shortcut == Cheap)
                , Event.onCheck (\_ -> Take Cheap)
                ]
                []
            , Html.label [ Attribute.for "cheap" ] [ Html.text "Cheap" ]
            ]
        , Html.div []
            [ run 1
            , run 10
            , run 100
            , run 1000
            ]
        ]


runOf : Model -> Int -> Html Msg
runOf (Model model) numberOfRuns =
    let
        suiteRunning =
            model.suite
                |> Maybe.map Suite.finished
                |> Maybe.withDefault False

        attributes =
            if not suiteRunning then
                [ Event.onClick <| Run numberOfRuns ]

            else
                []
    in
    Html.button attributes [ Html.text <| String.fromInt numberOfRuns ]
