module Visualizer.RandomExplore.Statistics exposing (Statistics, empty, failure, success, view)

import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)


type Statistics
    = Statistics
        { failedPaths : Dict Length Count
        , successfulPaths : Dict Length Count
        }


type alias Length =
    Int


type alias Count =
    Int


empty : Statistics
empty =
    Statistics
        { failedPaths = Dict.empty
        , successfulPaths = Dict.empty
        }


success : Length -> Statistics -> Statistics
success length (Statistics statistics) =
    let
        successes =
            statistics.successfulPaths
                |> increment length
    in
    Statistics { statistics | successfulPaths = successes }


failure : Length -> Statistics -> Statistics
failure length (Statistics statistics) =
    let
        failures =
            statistics.failedPaths
                |> increment length
    in
    Statistics { statistics | failedPaths = failures }


increment : Length -> Dict Length Count -> Dict Length Count
increment length pathsOfLength =
    let
        inc count =
            case count of
                Just c ->
                    Just <| c + 1

                Nothing ->
                    Just <| 1
    in
    Dict.update length inc pathsOfLength


view : Statistics -> Html msg
view ((Statistics s) as statistics) =
    Html.div []
        [ viewSummary statistics
        , viewCounts "Successfull" s.successfulPaths
        , viewCounts "Failed" s.failedPaths
        ]


viewSummary : Statistics -> Html msg
viewSummary (Statistics statistics) =
    let
        good =
            Dict.foldl (\_ v a -> v + a) 0 statistics.successfulPaths

        bad =
            Dict.foldl (\_ v a -> v + a) 0 statistics.failedPaths

        total =
            good + bad
    in
    Html.div []
        [ Html.span [] [ Html.text <| "Total: " ++ String.fromInt total ]
        , Html.span [] [ Html.text <| "Successful: " ++ String.fromInt good ]
        , Html.span [] [ Html.text <| "Failed: " ++ String.fromInt bad ]
        ]


viewCounts : String -> Dict Length Count -> Html msg
viewCounts name counts =
    let
        rows =
            counts
                |> Dict.toList
                |> List.sortBy Tuple.first
                |> List.map viewCountRow
    in
    Html.div []
        [ Html.h2 [] [ Html.text name ]
        , Html.table []
            [ Html.thead []
                [ Html.tr []
                    [ Html.th [] [ Html.text "Length" ]
                    , Html.th [] [ Html.text "Count" ]
                    ]
                ]
            , Html.tbody [] rows
            ]
        ]


viewCountRow : ( Length, Count ) -> Html msg
viewCountRow ( length, count ) =
    Html.tr []
        [ Html.td [] [ Html.text <| String.fromInt length ]
        , Html.td [] [ Html.text <| String.fromInt count ]
        ]
