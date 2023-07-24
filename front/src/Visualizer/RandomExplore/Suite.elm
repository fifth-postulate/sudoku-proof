module Visualizer.RandomExplore.Suite exposing (CellSelector, Model, Msg(..), create, finished, register, update)

import Random exposing (generate, uniform)
import Set exposing (Set)
import Sudoku exposing (Problem, candidatesAt, fill, options)
import Sudoku.Cell exposing (Cell)
import Sudoku.Domain exposing (Domain)
import Sudoku.Strategy as Strategy exposing (Strategy)
import Task
import Visualizer.RandomExplore.Statistics as Statistics exposing (Statistics)


type Model
    = Suite
        { problem : Problem
        , cheap : Strategy
        , cellSelector : CellSelector
        , remainingRuns : Int
        , resolutions : List Resolution
        }
    | Finished (List Resolution)


type alias CellSelector =
    List ( Cell, Set Domain ) -> List ( Cell, Set Domain )


create : Strategy -> CellSelector -> Int -> Problem -> Model
create strategy cellSelector remainingRuns problem =
    Suite
        { problem = problem
        , cheap = strategy
        , cellSelector = cellSelector
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
                ( Suite { suite | remainingRuns = suite.remainingRuns - 1 }, do <| Examine 0 (tryStrategy suite.cheap <| Just suite.problem) )

            else
                ( Finished suite.resolutions, do Done )

        ( Examine depth (Just problem), Suite suite ) ->
            if Sudoku.isSolved problem then
                ( Suite { suite | resolutions = Success depth :: suite.resolutions }, do Start )

            else if Sudoku.isOverConstrained problem then
                ( Suite { suite | resolutions = Failed depth :: suite.resolutions }, do Start )

            else
                case selectCells suite.cellSelector problem of
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
            ( model, do <| Examine (depth + 1) (tryStrategy suite.cheap result) )

        ( Done, _ ) ->
            ( model, Cmd.none )

        ( _, Finished _ ) ->
            ( model, Cmd.none )


tryStrategy : Strategy -> Maybe Problem -> Maybe Problem
tryStrategy strategy problem =
    let
        try : Problem -> Maybe Problem
        try p =
            case strategy p of
                Just plan ->
                    Strategy.execute plan p

                Nothing ->
                    Just p
    in
    Maybe.andThen try problem


selectCells : CellSelector -> Problem -> List Cell
selectCells selector problem =
    options problem
        |> selector
        |> List.map Tuple.first


do : Msg -> Cmd Msg
do msg =
    Task.perform identity <| Task.succeed msg
