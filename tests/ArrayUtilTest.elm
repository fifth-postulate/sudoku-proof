module ArrayUtilTest exposing (..)

import Array
import Array.Util as Util
import Expect
import Fuzz exposing (array, constant, intRange)
import Random
import Test exposing (..)


suite : Test
suite =
    describe "Array"
        [ describe "Util"
            [ describe "all"
                [ fuzz (array <| constant True) "of identity with an array of True values is True" <|
                    \subject ->
                        Expect.true "identity" (Util.all identity subject)
                , fuzz (array <| intRange 1 Random.maxInt) "of isPositive with array of positive integers is True" <|
                    \subject ->
                        Expect.true "isPositive" (Util.all (\n -> n > 0) subject)
                ]
            , describe "indexedFoldl"
                [ fuzz (array <| constant 1) "of sum works correctly" <|
                    \subject ->
                        Expect.equal (Array.length subject) <| Util.indexedFoldl (\( _, value ) acc -> acc + value) 0 subject
                , fuzz (array <| constant 1) "of weighted sum works correctly" <|
                    \subject ->
                        let
                            n =
                                Array.length subject

                            expected =
                                (n * (n - 1)) // 2
                        in
                        Expect.equal expected <| Util.indexedFoldl (\( weight, value ) acc -> acc + weight * value) 0 subject
                ]
            ]
        ]
