module Visualizer.RandomExplore exposing (Model, Msg, fromProblem, toProblem, update, view)

import Html.Styled as Html exposing (Html)
import Html.Styled.Events as Event
import Sudoku exposing (Problem)
import Task
import Visualizer.RandomExplore.Statistics as Statistics exposing (Statistics)
import Visualizer.RandomExplore.Suite as Suite exposing (Model, Msg(..), create)


type Model
    = Model
        { info : Sudoku.Info
        , problem : Problem
        , statistics : Statistics
        , suite : Maybe Suite.Model
        }


fromProblem : Sudoku.Info -> Problem -> Model
fromProblem info problem =
    Model
        { info = info
        , problem = problem
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        Run numberOfRuns ->
            ( Model { model | suite = Just <| create numberOfRuns model.problem }, Task.perform SuiteMsg <| Task.succeed Start )

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


view : Model -> Html Msg
view ((Model m) as model) =
    Html.div []
        [ viewControl model
        , Statistics.view m.statistics
        ]


viewControl : Model -> Html Msg
viewControl model =
    let
        run =
            runOf model
    in
    Html.div []
        [ run 1
        , run 10
        , run 100
        , run 1000
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
