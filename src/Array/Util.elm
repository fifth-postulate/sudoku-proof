module Array.Util exposing (all)

import Array exposing (Array)


all : (a -> Bool) -> Array a -> Bool
all transform array =
    array
        |> Array.map transform
        |> Array.foldl (&&) True
