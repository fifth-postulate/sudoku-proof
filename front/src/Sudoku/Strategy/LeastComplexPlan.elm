module Sudoku.Strategy.LeastComplexPlan exposing (strategy)

import PriorityQueue exposing (PriorityQueue)
import Set
import Sudoku exposing (Action, Problem, execute, isOverConstrained, isSolved)
import Sudoku.Strategy exposing (Plan, Strategy)


strategy : (Plan -> Int) -> Strategy
strategy priority problem =
    let
        queue =
            PriorityQueue.empty priority
                |> PriorityQueue.insert seed
    in
    firstSuggestionFromQueue queue problem


seed : Plan
seed =
    []


firstSuggestionFromQueue : PriorityQueue Plan -> Strategy
firstSuggestionFromQueue queue problem =
    let
        candidatePlan =
            PriorityQueue.head queue

        remaining =
            PriorityQueue.tail queue
    in
    case candidatePlan of
        Just plan ->
            case verdict problem plan of
                Solved ->
                    Just plan

                Indeterminate followups ->
                    let
                        cheap =
                            followups
                                |> List.filter (Tuple.second >> (==) 1)
                                |> List.map Tuple.first

                        costly =
                            followups
                                |> List.filter (Tuple.second >> (<) 1)
                                |> List.map Tuple.first

                        augmentedQueue =
                            case cheap of
                                c :: _ ->
                                    PriorityQueue.insert (plan ++ List.singleton c) remaining

                                [] ->
                                    costly
                                        |> List.map (List.singleton >> List.append plan)
                                        |> List.foldr PriorityQueue.insert remaining
                    in
                    firstSuggestionFromQueue augmentedQueue problem

                Unsolvable ->
                    firstSuggestionFromQueue remaining problem

        Nothing ->
            Nothing


type Verdict
    = Solved
    | Indeterminate (List ( Action, Int ))
    | Unsolvable


verdict : Problem -> Plan -> Verdict
verdict problem plan =
    let
        result =
            plan
                |> List.foldl (lift execute) (Just problem)

        indeterminate aProblem =
            let
                toAction ( cell, options ) =
                    options
                        |> Set.toList
                        |> List.map (\option -> ( Sudoku.fill cell option, Set.size options ))

                followups =
                    aProblem
                        |> Sudoku.options
                        |> List.concatMap toAction
            in
            Indeterminate followups

        judge r =
            if isSolved r then
                Solved

            else if isOverConstrained r then
                Unsolvable

            else
                indeterminate r
    in
    Maybe.map judge result
        |> Maybe.withDefault Unsolvable


lift : (Action -> Problem -> Maybe Problem) -> Action -> Maybe Problem -> Maybe Problem
lift f a problem =
    Maybe.andThen (f a) problem
