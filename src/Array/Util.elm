module Array.Util exposing (all, indexedFoldl, member)

import Array exposing (Array)


all : (a -> Bool) -> Array a -> Bool
all transform array =
    array
        |> Array.map transform
        |> Array.foldl (&&) True


indexedFoldl : (( Int, a ) -> b -> b) -> b -> Array a -> b
indexedFoldl transform seed array =
    array
        |> Array.indexedMap (\index value -> ( index, value ))
        |> Array.foldl transform seed


member : a -> Array a -> Bool
member =
    memberFrom 0


memberFrom : Int -> a -> Array a -> Bool
memberFrom index needle haystack =
    if index < Array.length haystack then
        if Array.get index haystack == Just needle then
            True

        else
            memberFrom (index + 1) needle haystack

    else
        False
