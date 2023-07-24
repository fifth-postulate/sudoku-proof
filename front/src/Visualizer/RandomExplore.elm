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
import Visualizer.RandomExplore.CellSelector as CellSelector
import Visualizer.RandomExplore.Statistics as Statistics exposing (Statistics)
import Visualizer.RandomExplore.Suite as Suite exposing (Model, Msg(..), create)


type Model
    = Model
        { info : Sudoku.Info
        , problem : Problem
        , shortcut : ShortCut
        , preference : CellPreference
        , statistics : Statistics
        , suite : Maybe Suite.Model
        }


type ShortCut
    = None
    | Naked
    | Hidden
    | NakedHidden
    | HiddenNaked


type CellPreference
    = Indifferent
    | LeastCandidates
    | MostCandidates


fromProblem : Sudoku.Info -> Problem -> Model
fromProblem info problem =
    Model
        { info = info
        , problem = problem
        , shortcut = NakedHidden
        , preference = Indifferent
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
    | Prefer CellPreference


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        Run numberOfRuns ->
            let
                strategy =
                    case model.shortcut of
                        None ->
                            None.strategy

                        Naked ->
                            repeated NakedSingle.strategy

                        Hidden ->
                            repeated HiddenSingle.strategy

                        NakedHidden ->
                            repeated (either [ NakedSingle.strategy, HiddenSingle.strategy ])

                        HiddenNaked ->
                            repeated (either [ HiddenSingle.strategy, NakedSingle.strategy ])

                cellSelector =
                    case model.preference of
                        Indifferent ->
                            CellSelector.indifferent

                        LeastCandidates ->
                            CellSelector.leastCandidates

                        MostCandidates ->
                            CellSelector.mostCandidates
            in
            ( Model { model | suite = Just <| create strategy cellSelector numberOfRuns model.problem }, Task.perform SuiteMsg <| Task.succeed Start )

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

        Prefer preference ->
            ( Model { model | preference = preference }, Cmd.none )


view : Model -> Html Msg
view ((Model m) as model) =
    Html.div []
        [ viewControl model
        , Statistics.view m.statistics
        ]


viewControl : Model -> Html Msg
viewControl model =
    Html.div []
        [ viewShortcutControl model
        , viewPreferenceControl model
        , viewRunControl model
        ]


viewShortcutControl : Model -> Html Msg
viewShortcutControl (Model m) =
    Html.div []
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
            , Attribute.id "naked"
            , Attribute.value "naked"
            , Attribute.checked (m.shortcut == Naked)
            , Event.onCheck (\_ -> Take Naked)
            ]
            []
        , Html.label [ Attribute.for "naked" ] [ Html.text "Naked" ]
        , Html.input
            [ Attribute.type_ "radio"
            , Attribute.name "shortcut"
            , Attribute.id "hidden"
            , Attribute.value "hidden"
            , Attribute.checked (m.shortcut == Hidden)
            , Event.onCheck (\_ -> Take Hidden)
            ]
            []
        , Html.label [ Attribute.for "hidden" ] [ Html.text "Hidden" ]
        , Html.input
            [ Attribute.type_ "radio"
            , Attribute.name "shortcut"
            , Attribute.id "nakedhidden"
            , Attribute.value "nakedhidden"
            , Attribute.checked (m.shortcut == NakedHidden)
            , Event.onCheck (\_ -> Take NakedHidden)
            ]
            []
        , Html.label [ Attribute.for "nakedhidden" ] [ Html.text "NakedHidden" ]
        , Html.input
            [ Attribute.type_ "radio"
            , Attribute.name "shortcut"
            , Attribute.id "hiddennaked"
            , Attribute.value "hiddennaked"
            , Attribute.checked (m.shortcut == HiddenNaked)
            , Event.onCheck (\_ -> Take HiddenNaked)
            ]
            []
        , Html.label [ Attribute.for "hiddennaked" ] [ Html.text "HiddenNaked" ]
        ]


viewPreferenceControl : Model -> Html Msg
viewPreferenceControl (Model m) =
    Html.div []
        [ Html.input
            [ Attribute.type_ "radio"
            , Attribute.name "cell-preference"
            , Attribute.id "indifferent"
            , Attribute.value "indifferent"
            , Attribute.checked (m.preference == Indifferent)
            , Event.onCheck (\_ -> Prefer Indifferent)
            ]
            []
        , Html.label [ Attribute.for "indifferent" ] [ Html.text "Indifferent" ]
        , Html.input
            [ Attribute.type_ "radio"
            , Attribute.name "cell-preference"
            , Attribute.id "least"
            , Attribute.value "least"
            , Attribute.checked (m.preference == LeastCandidates)
            , Event.onCheck (\_ -> Prefer LeastCandidates)
            ]
            []
        , Html.label [ Attribute.for "least" ] [ Html.text "Least" ]
        , Html.input
            [ Attribute.type_ "radio"
            , Attribute.name "cell-preference"
            , Attribute.id "most"
            , Attribute.value "most"
            , Attribute.checked (m.preference == MostCandidates)
            , Event.onCheck (\_ -> Prefer MostCandidates)
            ]
            []
        , Html.label [ Attribute.for "most" ] [ Html.text "Most" ]
        ]


viewRunControl : Model -> Html Msg
viewRunControl model =
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
