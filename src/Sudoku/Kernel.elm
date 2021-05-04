module Sudoku.Kernel exposing (Problem, Rule, Strategy, Suggestion, blockRule, cellRule, emptySudoku, execute, hint, isSolved, shouldBe, suggest)

import Array exposing (Array)
import Array.Util exposing (all, indexedFoldl)
import Maybe.Util exposing (orElse)
import Set exposing (Set)
import Sudoku.Blocks as Blocks


type Problem
    = Problem { states : Array State, blocks : List (Set Cell) }


type State
    = Determined Domain
    | Candidates (Set Domain)


type alias Domain =
    Int


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

                        Candidates candidates ->
                            candidates
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
    List Rule


type Rule
    = NoBlock (Set Domain -> Maybe Suggestion)
    | SingleBlock (Set Domain -> Maybe Suggestion)


type Suggestion
    = ShouldBe Domain


shouldBe : Domain -> Suggestion
shouldBe =
    ShouldBe


actOn : Cell -> Suggestion -> Action
actOn cell (ShouldBe d) =
    Fill cell d


cellRule : (Set Domain -> Maybe Suggestion) -> Rule
cellRule =
    NoBlock


blockRule : (Set Domain -> Maybe Suggestion) -> Rule
blockRule =
    SingleBlock


suggest : Strategy -> Problem -> Maybe Action
suggest strategy problem =
    case strategy of
        [] ->
            Nothing

        rule :: tail ->
            suggestFromRule rule problem
                |> orElse (\_ -> suggest tail problem)


suggestFromRule : Rule -> Problem -> Maybe Action
suggestFromRule rule (Problem { states }) =
    case rule of
        NoBlock suggestion ->
            let
                pickFirstActionableSuggestion : ( Domain, State ) -> Maybe Action -> Maybe Action
                pickFirstActionableSuggestion ( cell, currentState ) proposedAction =
                    proposedAction
                        |> orElse (\_ -> lift suggestion currentState |> Maybe.map (actOn cell))
            in
            indexedFoldl pickFirstActionableSuggestion Nothing states

        SingleBlock _ ->
            Nothing


lift : (Set Domain -> Maybe Suggestion) -> State -> Maybe Suggestion
lift f state =
    case state of
        Determined _ ->
            Nothing

        Candidates candidates ->
            f candidates
