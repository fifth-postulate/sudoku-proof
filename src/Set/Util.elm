module Set.Util exposing (pick)

import Set exposing (Set)


pick : Set a -> Maybe a
pick collection =
    collection
        |> Set.toList
        |> List.head
