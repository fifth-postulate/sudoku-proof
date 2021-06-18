module StreamTest exposing (..)

import Expect
import Stream exposing (Stream)
import Test exposing (..)


suite =
    describe "Stream"
        [ describe "constant stream" <|
            [ test "head of constant stream is constant value" <|
                \_ ->
                    let
                        actual =
                            Stream.constant 0
                                |> Stream.head
                                |> Maybe.map Tuple.first
                    in
                    Expect.equal (Just 0) actual
            , test "head of head of constant stream is still constant value" <|
                \_ ->
                    let
                        actual =
                            Stream.constant 0
                                |> Stream.head
                                |> Maybe.map Tuple.second
                                |> Maybe.andThen Stream.head
                                |> Maybe.map Tuple.first
                    in
                    Expect.equal (Just 0) actual
            ]
        , describe "integer stream" <|
            [ test "head of natural numbers is 0" <|
                \_ ->
                    let
                        actual =
                            naturalNumbers
                                |> Stream.head
                                |> Maybe.map Tuple.first
                    in
                    Expect.equal (Just 0)
                        actual
            , test "head of the head of natural numbers is 1" <|
                \_ ->
                    let
                        actual =
                            naturalNumbers
                                |> Stream.head
                                |> Maybe.map Tuple.second
                                |> Maybe.andThen Stream.head
                                |> Maybe.map Tuple.first
                    in
                    Expect.equal (Just 1) actual
            ]
        , describe "zip" <|
            [ test "head of zip of two infinite list" <|
                \_ ->
                    let
                        actual =
                            Stream.zip naturalNumbers naturalNumbers
                                |> Stream.head
                                |> Maybe.map Tuple.first
                    in
                    Expect.equal (Just ( 0, 0 )) actual
            , test "head of head zip of two infinite list" <|
                \_ ->
                    let
                        actual =
                            Stream.zip naturalNumbers naturalNumbers
                                |> Stream.head
                                |> Maybe.map Tuple.second
                                |> Maybe.andThen Stream.head
                                |> Maybe.map Tuple.first
                    in
                    Expect.equal (Just ( 1, 1 )) actual
            ]
        , describe "zipList" <|
            [ test "head of zipList of few infinite list" <|
                \_ ->
                    let
                        actual =
                            Stream.zipList [ naturalNumbers, Stream.map ((*) 2) naturalNumbers, Stream.map ((*) 3) naturalNumbers ]
                                |> Stream.head
                                |> Maybe.map Tuple.first
                    in
                    Expect.equal (Just [ 0, 0, 0 ]) actual
            , test "head of head zip of few infinite list" <|
                \_ ->
                    let
                        actual =
                            Stream.zipList [ naturalNumbers, Stream.map ((*) 2) naturalNumbers, Stream.map ((*) 3) naturalNumbers ]
                                |> Stream.head
                                |> Maybe.map Tuple.second
                                |> Maybe.andThen Stream.head
                                |> Maybe.map Tuple.first
                    in
                    Expect.equal (Just [ 1, 2, 3 ]) actual
            ]
        ]


naturalNumbers : Stream Int
naturalNumbers =
    naturalNumbersFrom 0


naturalNumbersFrom : Int -> Stream Int
naturalNumbersFrom n =
    Stream.singleton n
        |> Stream.afterwards (\_ -> naturalNumbersFrom <| n + 1)
