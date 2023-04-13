module Visualizer.Tree.Statistics exposing (Statistics, determined, explored, starting, updateDepth, view)

import Html.Styled as Html exposing (Html)


type alias Statistics =
    { nodesVisited : Int
    , nodesExplored : Int
    , nodesDetermined : Int
    , maximumDepth : Int
    }


starting : Statistics
starting =
    { nodesVisited = 1
    , nodesExplored = 0
    , nodesDetermined = 0
    , maximumDepth = 0
    }


explored : Statistics -> Statistics
explored s =
    { s
        | nodesVisited = s.nodesVisited + 1
        , nodesExplored = s.nodesExplored + 1
    }


determined : Statistics -> Statistics
determined s =
    { s
        | nodesVisited = s.nodesVisited + 1
        , nodesDetermined = s.nodesDetermined + 1
    }


updateDepth : Int -> Statistics -> Statistics
updateDepth currentDepth s =
    let
        maximumDepth =
            max currentDepth s.maximumDepth
    in
    { s | maximumDepth = maximumDepth }


view : Statistics -> Html msg
view statistics =
    Html.table []
        [ Html.thead []
            [ Html.tr []
                [ Html.th [] [ Html.text "Attribute" ]
                , Html.th [] [ Html.text "Value" ]
                ]
            ]
        , Html.tbody []
            [ Html.tr []
                [ Html.td [] [ Html.text "#nodes" ]
                , Html.td [] [ Html.text <| String.fromInt statistics.nodesVisited ]
                ]
            , Html.tr []
                [ Html.td [] [ Html.text "#explored" ]
                , Html.td [] [ Html.text <| String.fromInt statistics.nodesExplored ]
                ]
            , Html.tr []
                [ Html.td [] [ Html.text "#determined" ]
                , Html.td [] [ Html.text <| String.fromInt statistics.nodesDetermined ]
                ]
            , Html.tr []
                [ Html.td [] [ Html.text "max depth" ]
                , Html.td [] [ Html.text <| String.fromInt statistics.maximumDepth ]
                ]
            ]
        ]
