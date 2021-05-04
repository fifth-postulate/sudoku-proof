module SetUtilTest exposing (..)

import Expect
import Fuzz exposing (Fuzzer, int)
import Set exposing (Set)
import Set.Util exposing (pick)
import Test exposing (..)


suite : Test
suite =
    describe "Set"
        [ describe "Util"
            [ describe "pick"
                [ fuzz (Fuzz.map2 (\a b -> ( a, b )) (set int) int) "of non empty set is a Just" <|
                    \( base, element ) ->
                        let
                            subject =
                                Set.insert element base
                        in
                        Expect.true "non empty" <| isJust (pick subject)
                , test "of empty set is a Nothing" <|
                    \_ -> Expect.false "empty" <| isJust (pick Set.empty)
                ]
            ]
        ]


set : Fuzzer comparable -> Fuzzer (Set comparable)
set fuzzer =
    fuzzer
        |> Fuzz.list
        |> Fuzz.map Set.fromList


isJust : Maybe a -> Bool
isJust input =
    input
        |> Maybe.map (always True)
        |> Maybe.withDefault False
