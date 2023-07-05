module Visualizer.RandomExplore.Suite exposing (Model, Msg(..), create, finished, register, update)

import Random exposing (generate, uniform)
import Set
import Sudoku exposing (Problem, candidatesAt, cellOptions, fill)
import Sudoku.Cell exposing (Cell)
import Sudoku.Domain exposing (Domain)
import Task
import Visualizer.RandomExplore.Statistics as Statistics exposing (Statistics)


type Model
    = Suite
        { problem : Problem
        , remainingRuns : Int
        , resolutions : List Resolution
        }
    | Finished (List Resolution)


create : Int -> Problem -> Model
create remainingRuns problem =
    Suite
        { problem = problem
        , remainingRuns = remainingRuns
        , resolutions = []
        }


finished : Model -> Bool
finished s =
    case s of
        Finished _ ->
            True

        _ ->
            False


register : Statistics -> Model -> Statistics
register statistics model =
    let
        updateStatistics r s =
            case r of
                Success depth ->
                    Statistics.success depth s

                Failed depth ->
                    Statistics.failure depth s
    in
    case model of
        Finished resolutions ->
            List.foldl updateStatistics statistics resolutions

        _ ->
            statistics


type Resolution
    = Success Int
    | Failed Int


type Msg
    = Start
    | Examine Int (Maybe Problem)
    | CellPicked Int Problem Cell
    | CandidatePicked Int Problem Cell Domain
    | Done


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Start, Suite suite ) ->
            if suite.remainingRuns > 0 then
                -- TODO apply cheap strategy
                ( Suite { suite | remainingRuns = suite.remainingRuns - 1 }, do <| Examine 0 (Just suite.problem) )

            else
                ( Finished suite.resolutions, do Done )

        ( Examine depth (Just problem), Suite suite ) ->
            if Sudoku.isSolved problem then
                ( Suite { suite | resolutions = Success depth :: suite.resolutions }, do Start )

            else if Sudoku.isOverConstrained problem then
                ( Suite { suite | resolutions = Failed depth :: suite.resolutions }, do Start )

            else
                case cellOptions problem of
                    c :: cs ->
                        ( model, generate (CellPicked depth problem) <| uniform c cs )

                    [] ->
                        ( Suite { suite | resolutions = Failed depth :: suite.resolutions }, do Start )

        ( Examine depth Nothing, Suite suite ) ->
            ( Suite { suite | resolutions = Failed depth :: suite.resolutions }, do Start )

        ( CellPicked depth problem cell, Suite suite ) ->
            let
                candidates =
                    candidatesAt cell problem
                        |> Set.toList
            in
            case candidates of
                c :: cs ->
                    ( model, generate (CandidatePicked depth problem cell) <| uniform c cs )

                [] ->
                    ( Suite { suite | resolutions = Failed depth :: suite.resolutions }, do Start )

        ( CandidatePicked depth problem cell candidate, Suite suite ) ->
            let
                action =
                    fill cell candidate

                result =
                    Sudoku.execute action problem
            in
            -- TODO apply cheap strategy
            ( model, do <| Examine (depth + 1) result )

        ( Done, _ ) ->
            ( model, Cmd.none )

        ( _, Finished _ ) ->
            ( model, Cmd.none )


do : Msg -> Cmd Msg
do msg =
    Task.perform identity <| Task.succeed msg
