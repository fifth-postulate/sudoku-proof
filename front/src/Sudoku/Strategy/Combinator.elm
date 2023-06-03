module Sudoku.Strategy.Combinator exposing (either, repeated)

import Sudoku.Strategy as Strategy exposing (Plan, Strategy)


either : List Strategy -> Strategy
either =
    tailrec_either Nothing


tailrec_either : Maybe Plan -> List Strategy -> Strategy
tailrec_either plan strategies problem =
    case plan of
        Just _ ->
            plan

        Nothing ->
            case strategies of
                s :: ss ->
                    tailrec_either (s problem) ss problem

                [] ->
                    Nothing


repeated : Strategy -> Strategy
repeated =
    tailrec_repeated []


tailrec_repeated : Plan -> Strategy -> Strategy
tailrec_repeated plan strategy problem =
    case strategy problem of
        Just continuedPlan ->
            case Strategy.execute continuedPlan problem of
                Just p ->
                    tailrec_repeated (plan ++ continuedPlan) strategy p

                Nothing ->
                    -- This should never occur
                    Just plan

        Nothing ->
            if not <| List.isEmpty plan then
                Just plan

            else
                Nothing
