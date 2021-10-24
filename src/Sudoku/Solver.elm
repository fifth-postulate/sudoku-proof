module Sudoku.Solver exposing (Plan, Strategy, complexity, solve)

import Fuel exposing (Fuel(..), consume)
import PriorityQueue exposing (PriorityQueue)
import Set
import Sudoku exposing (Action, Problem, execute, isOverConstrained, isSolved)


type alias Strategy =
    Problem -> Maybe Plan


solve : Strategy
solve =
    solveWithFuel Infinite


solveWithFuel : Fuel -> Strategy
solveWithFuel fuel problem =
    let
        priority : Plan -> Int
        priority =
            complexity

        queue =
            PriorityQueue.empty priority
                |> PriorityQueue.insert seed
    in
    firstSuggestionFromQueue fuel queue problem


complexity : Plan -> Int
complexity plan =
    plan
        |> List.map Tuple.second
        |> List.foldl (*) 1


type alias Plan =
    List ( Action, Int )


seed : Plan
seed =
    []


firstSuggestionFromQueue : Fuel -> PriorityQueue Plan -> Strategy
firstSuggestionFromQueue fuel queue problem =
    let
        plan =
            PriorityQueue.head queue

        remaining =
            PriorityQueue.tail queue
    in
    plan
        |> Maybe.andThen (firstSuggestionFromPlan fuel problem remaining)


firstSuggestionFromPlan : Fuel -> Problem -> PriorityQueue Plan -> Plan -> Maybe Plan
firstSuggestionFromPlan fuel problem queue plan =
    case verdict problem plan of
        Solved ->
            Just plan

        Indeterminate followups ->
            let
                augmentedQueue =
                    followups
                        |> List.map (\followup -> plan ++ [ followup ])
                        |> List.foldl PriorityQueue.insert queue
            in
            consume fuel
                |> Maybe.andThen
                    (\f -> firstSuggestionFromQueue f augmentedQueue problem)

        Unsolvable _ ->
            consume fuel
                |> Maybe.andThen
                    (\f -> firstSuggestionFromQueue f queue problem)


type Verdict
    = Solved
    | Indeterminate (List ( Action, Int ))
    | Unsolvable Reason


type Reason
    = UnderConstrained
    | OverConstrained
    | LogicError String


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
        Unsolvable OverConstrained

    else
        indeterminate result
