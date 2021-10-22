module Fuel exposing (Fuel(..), consume)


type Fuel
    = Finite Int
    | Infinite


consume : Fuel -> Maybe Fuel
consume fuel =
    case fuel of
        Finite 0 ->
            Nothing

        Finite n ->
            Just <| Finite <| n - 1

        Infinite ->
            Just <| Infinite
