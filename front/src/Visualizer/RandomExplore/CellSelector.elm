module Visualizer.RandomExplore.CellSelector exposing (indifferent, leastCandidates, mostCandidates)

import Set exposing (Set)
import Visualizer.RandomExplore.Suite exposing (CellSelector)


indifferent : CellSelector
indifferent =
    identity


leastCandidates : CellSelector
leastCandidates candidates =
    candidates
        |> List.sortWith leastOrder
        |> takeSimilar


mostCandidates : CellSelector
mostCandidates candidates =
    candidates
        |> List.sortWith mostOrder
        |> takeSimilar


takeSimilar : List ( a, Set b ) -> List ( a, Set b )
takeSimilar original =
    case original of
        ( _, zs ) :: _ ->
            tailrec_takeSimilar (Set.size zs) [] original

        [] ->
            []


tailrec_takeSimilar : Int -> List ( a, Set b ) -> List ( a, Set b ) -> List ( a, Set b )
tailrec_takeSimilar target acc xs =
    case xs of
        (( _, zs ) as head) :: tail ->
            if target == Set.size zs then
                tailrec_takeSimilar target (head :: acc) tail

            else
                acc

        [] ->
            acc


leastOrder : ( a, Set b ) -> ( a, Set b ) -> Order
leastOrder ( _, xs ) ( _, ys ) =
    compare (Set.size xs) (Set.size ys)


mostOrder : ( a, Set b ) -> ( a, Set b ) -> Order
mostOrder ( _, xs ) ( _, ys ) =
    compare (Set.size ys) (Set.size xs)
