module Sudoku.Kernel exposing (Problem, Strategy, Suggestion, emptySudoku, execute, hint, isSolved, shouldBe, solve)

import Array exposing (Array)
import Array.Util exposing (all)
import Debug
import Set exposing (Set)
import Set.Util exposing (pick)
import Stream.Kernel as Stream exposing (Stream)
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
    all isDetermined states


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


hint : Cell -> Domain -> Problem -> Problem
hint cell d =
    execute (Fill cell d)


execute : Action -> Problem -> Problem
execute (Fill cell d) (Problem problem) =
    let
        consequences =
            problem.blocks
                |> List.filter (Set.member cell)
                |> List.foldl Set.union Set.empty
                |> Set.remove cell
                |> Set.toList
                |> List.map (\c -> RemoveCandidateAt c d)
                |> (::) (Determine cell d)

        states =
            List.foldl apply problem.states consequences
    in
    Problem { problem | states = states }


type Action
    = Fill Cell Domain


apply : Consequence -> Array State -> Array State
apply consequence states =
    case consequence of
        Determine cell d ->
            states
                |> Array.set cell (Determined d)

        RemoveCandidateAt cell d ->
            let
                update : State -> State
                update state =
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
                        |> Maybe.map update
            in
            updatedState
                |> Maybe.map (\s -> Array.set cell s states)
                |> Maybe.withDefault states


type Consequence
    = Determine Cell Domain
    | RemoveCandidateAt Cell Domain


type alias Strategy =
    Problem -> Maybe Action


type Suggestion
    = ShouldBe Domain


shouldBe : Domain -> Suggestion
shouldBe =
    ShouldBe


actOn : Cell -> Suggestion -> Action
actOn cell (ShouldBe d) =
    Fill cell d


solve : Strategy
solve ((Problem { states }) as problem) =
    states
        |> toStream
        |> firstSuggestion problem


type Tree
    = Leaf Cell (Set Domain)
    | Node Cell (Set Domain) (List ( Block, List Tree ))


effectiveCandidates : Tree -> Set Domain
effectiveCandidates tree =
    case tree of
        Leaf _ domain ->
            domain

        Node _ domain children ->
            let
                exclude =
                    children
                        |> List.concatMap Tuple.second
                        |> List.map effectiveCandidates
                        |> List.foldl Set.union Set.empty
            in
            Set.diff domain exclude


rootCell : Tree -> Cell
rootCell tree =
    case tree of
        Leaf cell _ ->
            cell

        Node cell _ _ ->
            cell


toStream : Array State -> Stream Tree
toStream cells =
    cells
        |> Array.indexedMap Tuple.pair
        |> Array.filter (Tuple.second >> isDetermined >> not)
        |> Array.toList
        |> List.sortBy (Tuple.second >> candidates >> Set.size)
        |> List.map (Tuple.mapSecond candidates)
        |> List.map (uncurry Leaf)
        |> Stream.fromList


uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b


firstSuggestion : Problem -> Stream Tree -> Maybe Action
firstSuggestion problem stream =
    stream
        |> Stream.head
        |> Maybe.andThen (suggestionFromTree problem)


suggestionFromTree : Problem -> ( Tree, Stream Tree ) -> Maybe Action
suggestionFromTree problem ( tree, stream ) =
    let
        domain =
            effectiveCandidates tree
    in
    if Set.size domain == 1 then
        domain
            |> pick
            |> Maybe.map shouldBe
            |> Maybe.map (actOn <| rootCell tree)

    else
        stream
            |> Stream.afterwards (\_ -> sprout problem tree)
            |> firstSuggestion problem


sprout : Problem -> Tree -> Stream Tree
sprout ((Problem { blocks }) as problem) tree =
    case tree of
        Leaf cell domain ->
            blocks
                |> List.filter (Set.member cell)
                |> List.map (leafSproutPromise problem cell domain)
                |> List.foldl Stream.afterwards Stream.empty

        Node cell domain children ->
            Stream.empty


leafSproutPromise : Problem -> Cell -> Set Domain -> Block -> () -> Stream Tree
leafSproutPromise (Problem { states }) cell domain block =
    let
        trees =
            block
                |> Set.remove cell
                |> Set.filter hasCandidates
                |> Set.toList
                |> List.map toLeaf

        hasCandidates candidate =
            states
                |> Array.get candidate
                |> Maybe.map (not << isDetermined)
                |> Maybe.withDefault False

        toLeaf c =
            let
                d =
                    states
                        |> Array.get c
                        |> Maybe.map candidates
                        |> Maybe.withDefault Set.empty
            in
            Leaf c d
    in
    \_ -> Stream.singleton <| Node cell domain [ ( block, trees ) ]
