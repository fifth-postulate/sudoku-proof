module Sudoku exposing (Action, Fuel(..), Info, Msg(..), Problem, Strategy, Suggestion, clue, emptySudoku, execute, isSolved, shouldBe, solve, solveWithFuel, update, view, viewAction)

import Array exposing (Array)
import Array.Util as Util
import Css exposing (..)
import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attribute
import Set exposing (Set)
import Set.Util exposing (pick)
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
    Problem -> Maybe Action


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
    firstSuggestionFromStream fuel (Stream.singleton Seed) problem


type Tree
    = Node Cell (Dict Domain Tree)
    | Seed


root : Cell -> Set Domain -> Tree
root cell cs =
    let
        options =
            cs
                |> Set.toList
                |> List.map (\d -> ( d, Seed ))
                |> Dict.fromList
    in
    Node cell options


firstSuggestionFromStream : Fuel -> Stream Tree -> Strategy
firstSuggestionFromStream fuel stream problem =
    stream
        |> Stream.head
        |> Maybe.andThen (firstSuggestionFromTree fuel problem)


firstSuggestionFromTree : Fuel -> Problem -> ( Tree, Stream Tree ) -> Maybe Action
firstSuggestionFromTree fuel problem ( tree, stream ) =
    case verdict problem tree of
        Solvable action ->
            List.head action

        Indeterminate ->
            let
                augmentedStream =
                    stream
                        |> Stream.afterwards (\_ -> grow problem tree)
            in
            consume fuel
                |> Maybe.andThen
                    (\f -> firstSuggestionFromStream f augmentedStream problem)

        Unsolvable _ ->
            consume fuel
                |> Maybe.andThen
                    (\f -> firstSuggestionFromStream f stream problem)


type Verdict
    = Solvable (List Action)
    | Indeterminate
    | Unsolvable Reason


type Reason
    = UnderConstrained
    | OverConstrained
    | LogicError String


verdict : Problem -> Tree -> Verdict
verdict ((Problem { states }) as problem) tree =
    let
        anyStateWithNoCandidates =
            Util.any (\c -> (not <| isDetermined c) && (Set.isEmpty <| candidates c)) states
    in
    if anyStateWithNoCandidates then
        Unsolvable OverConstrained

    else
        case tree of
            Seed ->
                if isSolved problem then
                    Solvable []

                else
                    Indeterminate

            Node cell choices ->
                let
                    options : Set Domain
                    options =
                        states
                            |> Array.get cell
                            |> Maybe.map candidates
                            |> Maybe.withDefault Set.empty
                in
                case Set.size options of
                    0 ->
                        Unsolvable <| LogicError "a state with 0 options should be caught earlier"

                    _ ->
                        let
                            verdicts =
                                choices
                                    |> Dict.toList
                                    |> List.map (\( d, t ) -> ( d, verdict (execute (Fill cell d) problem) t ))

                            isSolvable v =
                                case v of
                                    Solvable _ ->
                                        True

                                    _ ->
                                        False

                            isUnsolvable v =
                                case v of
                                    Unsolvable _ ->
                                        True

                                    _ ->
                                        False

                            solvables =
                                verdicts
                                    |> List.filter (Tuple.second >> isSolvable)

                            unsolvables =
                                verdicts
                                    |> List.filter (Tuple.second >> isUnsolvable)
                        in
                        if List.length solvables >= 2 then
                            Unsolvable UnderConstrained

                        else if List.length solvables == 1 then
                            let
                                prependAction ( d, v ) =
                                    case v of
                                        Solvable actions ->
                                            Just <| Solvable <| Fill cell d :: actions

                                        _ ->
                                            Nothing
                            in
                            solvables
                                |> List.head
                                |> Maybe.andThen prependAction
                                |> Maybe.withDefault (Unsolvable <| LogicError "solvable should have a head because it has a lenght of 1")

                        else if List.length unsolvables == Dict.size choices - 1 then
                            let
                                badChoices =
                                    unsolvables
                                        |> List.map Tuple.first
                            in
                            verdicts
                                |> List.filter (\( d, _ ) -> not <| List.member d badChoices)
                                |> List.head
                                |> Maybe.map (\( d, _ ) -> Solvable <| List.singleton <| Fill cell d)
                                |> Maybe.withDefault (Unsolvable <| LogicError "verdicts should leave a head after filter because it has a one less bad choice then total choices")

                        else
                            Indeterminate


type Suggestion
    = ShouldBe Domain


shouldBe : Domain -> Suggestion
shouldBe =
    ShouldBe


actOn : Cell -> Suggestion -> Action
actOn cell (ShouldBe d) =
    Fill cell d


grow : Problem -> Tree -> Stream Tree
grow ((Problem { states }) as problem) tree =
    case tree of
        Seed ->
            states
                |> Array.toIndexedList
                |> List.filter (\( _, state ) -> not <| isDetermined state)
                |> List.map (Tuple.mapSecond candidates)
                |> List.sortWith candidateSizeIndex
                |> List.map (\( c, ds ) -> root c ds)
                |> Stream.fromList

        Node cell choices ->
            let
                streamAfterChoice : ( Domain, Tree ) -> Stream ( Domain, Tree )
                streamAfterChoice ( d, t ) =
                    let
                        action =
                            actOn cell (shouldBe d)

                        futureProblem =
                            execute action problem
                    in
                    case verdict futureProblem t of
                        Indeterminate ->
                            Stream.eventually (\_ -> grow futureProblem t)
                                |> Stream.map (Tuple.pair d)

                        _ ->
                            Stream.constant t
                                |> Stream.map (Tuple.pair d)
            in
            choices
                |> Dict.toList
                |> List.map streamAfterChoice
                |> Stream.zipList
                |> Stream.map Dict.fromList
                |> Stream.map (Node cell)


candidateSizeIndex : ( Int, Set Domain ) -> ( Int, Set Domain ) -> Order
candidateSizeIndex ( leftCell, leftCandidates ) ( rightCell, rightCandidates ) =
    case compare (Set.size leftCandidates) (Set.size rightCandidates) of
        EQ ->
            compare leftCell rightCell

        _ as order ->
            order



-- UPDATE


type Msg
    = Advance


update : Msg -> Problem -> ( Problem, Maybe Action )
update msg problem =
    let
        action =
            solve problem
    in
    case msg of
        Advance ->
            action
                |> Maybe.map (\a -> ( execute a problem, Just a ))
                |> Maybe.withDefault ( problem, Nothing )



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
