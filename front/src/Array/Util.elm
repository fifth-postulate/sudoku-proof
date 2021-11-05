module Array.Util exposing (all, any)

import Array exposing (Array)


all : (a -> Bool) -> Array a -> Bool
all transform array =
    array
        |> Array.map transform
        |> Array.foldl (&&) True


any : (a -> Bool) -> Array a -> Bool
any transform array =
    array
        |> Array.map transform
        |> Array.foldl (||) False
