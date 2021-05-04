module Maybe.Util exposing (orElse)


orElse : (() -> Maybe a) -> Maybe a -> Maybe a
orElse second first =
    case first of
        Just _ ->
            first

        _ ->
            second ()
