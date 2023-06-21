module Visualizer.RandomExplore.Suite exposing (Model, Msg(..), create, finished, register, update)

import Sudoku exposing (Problem)
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
    case model of
        Finished resolutions ->
            -- TODO
            statistics

        _ ->
            statistics


type alias Resolution =
    { name : String
    }


type Msg
    = Start
    | Finish


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
