module Stack exposing (Stack, empty, peek, pop, push)

import Html.Styled as Html exposing (Html)


type Stack a
    = Stack (List a)


empty : Stack alias
empty =
    Stack []


push : a -> Stack a -> Stack a
push c (Stack cs) =
    Stack <| c :: cs


pop : Stack a -> ( Maybe a, Stack a )
pop (Stack cs) =
    ( List.head cs, List.drop 1 cs |> Stack )


peek : Stack a -> Maybe a
peek (Stack cs) =
    List.head cs


view : (a -> Html msg) -> Stack a -> Html msg
view subView (Stack cs) =
    let
        frame a =
            Html.div []
                [ subView a
                ]
    in
    Html.div [] <| List.map frame cs
