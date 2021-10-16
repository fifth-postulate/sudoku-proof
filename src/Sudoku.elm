module Sudoku exposing (Action, Fuel(..), Info, Plan, Problem, Strategy, clue, emptySudoku, execute, isSolved, solve, solveWithFuel, view, viewAction)

import Array exposing (Array)
import Array.Util as Util
import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Set exposing (Set)
import Stream exposing (Stream)
import Sudoku.Blocks as Blocks


type Problem
    = Problem { states : Array State, blocks : List Block }


type State
    = Determined Domain
    | Candidates (Set Domain)


type alias Domain =
    Int


type alias Block =
    Set Cell


type alias Cell =
    Int


isSolved : Problem -> Bool
isSolved (Problem { states }) =
    Util.all isDetermined states


isDetermined : State -> Bool
isDetermined state =
    case state of
        Determined _ ->
            True

        _ ->
            False


candidates : State -> Set Domain
candidates state =
    case state of
        Determined _ ->
            Set.empty

        Candidates domain ->
            domain


emptySudoku : Int -> Problem
emptySudoku m =
    let
        n =
            m * m

        emptyState =
            m
                |> List.range 1
                |> Set.fromList
                |> Candidates

        states =
            emptyState
                |> Array.repeat n
    in
    Problem { states = states, blocks = Blocks.sudokuBlocks m }


clue : Cell -> Domain -> Problem -> Problem
clue cell d =
    execute (Fill cell d)


type Action
    = Fill Cell Domain


execute : Action -> Problem -> Problem
execute (Fill cell d) (Problem problem) =
    let
        consequences =
            problem.blocks
                |> List.filter (Set.member cell)
                |> List.foldl Set.union Set.empty
                |> Set.remove cell
                |> Set.toList
                |> List.map (RemoveCandidateAt d)
                |> (::) (Determine d cell)

        states =
            List.foldl apply problem.states consequences
    in
    Problem { problem | states = states }


type Consequence
    = Determine Domain Cell
    | RemoveCandidateAt Domain Cell


apply : Consequence -> Array State -> Array State
apply consequence states =
    case consequence of
        Determine d cell ->
            states
                |> Array.set cell (Determined d)

        RemoveCandidateAt d cell ->
            let
                updateState : State -> State
                updateState state =
                    case state of
                        Determined v ->
                            Determined v

                        Candidates domain ->
                            domain
                                |> Set.remove d
                                |> Candidates

                updatedState =
                    states
                        |> Array.get cell
                        |> Maybe.map updateState
            in
            updatedState
                |> Maybe.map (\s -> Array.set cell s states)
                |> Maybe.withDefault states


type alias Strategy =
    Problem -> Maybe Plan


type Fuel
    = Finite Int
    | Infinite


consume : Fuel -> Maybe Fuel
consume fuel =
    case fuel of
        Finite 0 ->
            Nothing

        Finite n ->
            Just <| Finite <| n - 1

        Infinite ->
            Just <| Infinite


solve : Strategy
solve =
    solveWithFuel Infinite


solveWithFuel : Fuel -> Strategy
solveWithFuel fuel problem =
    firstSuggestionFromStream fuel (Stream.singleton seed) problem


type alias Plan =
    List Action


seed : Plan
seed =
    []


firstSuggestionFromStream : Fuel -> Stream Plan -> Strategy
firstSuggestionFromStream fuel stream problem =
    stream
        |> Stream.head
        |> Maybe.andThen (firstSuggestionFromPlan fuel problem)


firstSuggestionFromPlan : Fuel -> Problem -> ( Plan, Stream Plan ) -> Maybe Plan
firstSuggestionFromPlan fuel problem ( plan, stream ) =
    case verdict problem plan of
        Solved ->
            Just plan

        Indeterminate followups ->
            let
                augmentedStream =
                    followups
                        |> List.map (\action -> plan ++ [ action ])
                        |> List.map Stream.singleton
                        |> List.map (\s -> \_ -> s)
                        |> List.foldl Stream.afterwards stream
            in
            consume fuel
                |> Maybe.andThen
                    (\f -> firstSuggestionFromStream f augmentedStream problem)

        Unsolvable _ ->
            consume fuel
                |> Maybe.andThen
                    (\f -> firstSuggestionFromStream f stream problem)


type Verdict
    = Solved
    | Indeterminate (List Action)
    | Unsolvable Reason


type Reason
    = UnderConstrained
    | OverConstrained
    | LogicError String


verdict : Problem -> Plan -> Verdict
verdict problem plan =
    let
        result =
            List.foldl execute problem plan

        hasStatesWithNoCandidates (Problem { states }) =
            Util.any (\c -> (not <| isDetermined c) && (Set.isEmpty <| candidates c)) states

        indeterminate (Problem { states }) =
            let
                toAction ( cell, options ) =
                    options
                        |> Set.toList
                        |> List.map (\option -> Fill cell option)

                followups =
                    states
                        |> Array.indexedMap (\cell state -> ( cell, candidates state ))
                        |> Array.toList
                        |> List.filter (\( _, options ) -> 0 < Set.size options)
                        |> List.concatMap toAction
            in
            Indeterminate followups
    in
    if isSolved result then
        Solved

    else if hasStatesWithNoCandidates result then
        Unsolvable OverConstrained

    else
        indeterminate result



-- VIEW


type alias Info =
    { m : Int }


view : Info -> Problem -> Html msg
view { m } ((Problem { states }) as problem) =
    states
        |> Array.indexedMap (viewCell m problem)
        |> Array.toList
        |> Html.div
            [ Attribute.css
                [ grid
                , gridTemplateColumns <| List.repeat m "100px"
                , gridAutoRows "100px"
                ]
            ]


grid : Style
grid =
    property "display" "grid"


gridTemplateColumns : List String -> Style
gridTemplateColumns dimensions =
    property "grid-template-columns" <| String.join " " dimensions


gridAutoRows : String -> Style
gridAutoRows size =
    property "grid-auto-rows" size


viewCell : Int -> Problem -> Int -> State -> Html msg
viewCell m problem index cell =
    let
        content =
            case cell of
                Determined v ->
                    String.fromInt v

                Candidates cs ->
                    cs
                        |> Set.toList
                        |> List.map String.fromInt
                        |> List.sort
                        |> String.join ","

        color =
            case cell of
                Determined _ ->
                    rgb 0 0 0

                Candidates _ ->
                    rgb 200 200 200

        size =
            case cell of
                Determined _ ->
                    medium

                Candidates _ ->
                    xxSmall
    in
    Html.div
        [ Attribute.css
            [ borderColor (rgb 192 192 192)
            , borderWidth <| px 1
            , borderStyle solid
            , displayFlex
            , justifyContent center
            , alignItems center
            , Css.color color
            , fontSize size
            ]
        ]
        [ Html.text content ]


viewAction : Action -> Html msg
viewAction (Fill cell domain) =
    let
        message =
            String.join " " [ "write", String.fromInt domain, "in cell", String.fromInt cell ]
    in
    Html.span [] [ Html.text message ]
