module Array.Util exposing (all, indexedFoldl)

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
