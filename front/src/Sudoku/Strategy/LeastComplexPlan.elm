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

                        costly =
                            followups
                                |> List.filter (Tuple.second >> (<) 1)

                        augmentedQueue =
                            case List.head cheap of
                                Just a ->
                                    PriorityQueue.insert (plan ++ [ a ]) remaining

                                Nothing ->
                                    costly
                                        |> List.map (\followup -> plan ++ [ followup ])
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
                |> List.map Tuple.first
                |> List.foldl execute problem

        indeterminate aProblem =
            let
                toAction ( cell, options ) =
                    options
                        |> Set.toList
                        |> List.map (\option -> ( Sudoku.fill cell option, Set.size options ))

                followups =
                    aProblem
                        |> Sudoku.options
                        |> List.filter (\( _, options ) -> 0 < Set.size options)
                        |> List.concatMap toAction
            in
            Indeterminate followups
    in
    if isSolved result then
        Solved

    else if isOverConstrained result then
        Unsolvable

    else
        indeterminate result
