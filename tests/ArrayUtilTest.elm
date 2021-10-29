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
            , describe "any"
                [ fuzz (array <| constant True) "of identity with an non empty array of True values is True" <|
                    \subject ->
                        Expect.true "identity" <| (Array.length subject == 0) || (Util.any identity <| subject)
                , fuzz (array <| intRange 1 Random.maxInt) "of isPositive with array of positive integers is True" <|
                    \subject ->
                        Expect.true "isPositive" <| (Array.length subject == 0) || Util.any (\n -> n > 0) subject
                ]
            ]
        ]
