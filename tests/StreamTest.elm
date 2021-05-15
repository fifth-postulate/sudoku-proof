module StreamTest exposing (..)

import Expect
import Fuzz exposing (int, list)
import Stream exposing (Stream)
import Test exposing (..)


suite =
    describe "Stream"
        [ describe "integer stream" <|
            [ test "head of natural numbers is 0" <|
                \_ ->
                    let
                        actual =
                            naturalNumbers
                                |> Stream.head
                                |> Maybe.map (\( n, _ ) -> n)
                    in
                    Expect.equal (Just 0)
                        actual
            , test "head of the head of natural numbers is 1" <|
                \_ ->
                    let
                        actual =
                            naturalNumbers
                                |> Stream.head
                                |> Maybe.map (\( _, stream ) -> stream)
                                |> Maybe.andThen Stream.head
                                |> Maybe.map (\( n, _ ) -> n)
                    in
                    Expect.equal (Just 1) actual
            ]
        ]


naturalNumbers : Stream Int
naturalNumbers =
    naturalNumbersFrom 0


naturalNumbersFrom : Int -> Stream Int
naturalNumbersFrom n =
    Stream.singleton n
        |> Stream.afterwards (\_ -> naturalNumbersFrom <| n + 1)
