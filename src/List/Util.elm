module List.Util exposing (cartesianProduct)


cartesianProduct : List a -> List b -> List ( a, b )
cartesianProduct vs ws =
    vs
        |> List.concatMap (pair ws)


pair : List b -> a -> List ( a, b )
pair ws v =
    zip (List.repeat (List.length ws) v) ws


zip vs ws =
    case ( vs, ws ) of
        ( x :: xs, y :: ys ) ->
            ( x, y ) :: zip xs ys

        _ ->
            []
