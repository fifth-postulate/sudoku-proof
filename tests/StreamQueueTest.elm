module StreamQueueTest exposing (..)

import Expect
import Fuzz exposing (int, list)
import Stream.Queue as Queue
import Test exposing (..)


suite =
    describe "Stream"
        [ describe "Queue"
            [ fuzz (list int) "fromList toList are inverses" <|
                \expected ->
                    Expect.equal expected <| (Queue.toList << Queue.fromList) expected
            , fuzz (Fuzz.map2 (\a b -> ( a, b )) (list int) (list int)) "append fromList commutes" <|
                \( l, r ) ->
                    let
                        left =
                            Queue.fromList l

                        right =
                            Queue.fromList r

                        expected =
                            Queue.fromList <| List.append l r

                        actual =
                            Queue.append left right
                    in
                    Expect.equal (Queue.toList expected) (Queue.toList actual)
            , fuzz (Fuzz.map2 (\a b -> ( a, b )) int (list int)) "pushFront behaves" <|
                \( x, xs ) ->
                    let
                        expected =
                            Queue.fromList <| x :: xs

                        actual =
                            Queue.pushFront x <| Queue.fromList xs
                    in
                    Expect.equal (Queue.toList expected) (Queue.toList actual)
            , fuzz (Fuzz.map2 (\a b -> ( a, b )) int (list int)) "pushBack behaves" <|
                \( x, xs ) ->
                    let
                        expected =
                            Queue.fromList <| List.append xs [ x ]

                        actual =
                            Queue.pushBack x <| Queue.fromList xs
                    in
                    Expect.equal (Queue.toList expected) (Queue.toList actual)
            , fuzz (Fuzz.map2 (\a b -> ( a, b )) int (list int)) "popFront behaves" <|
                \( x, xs ) ->
                    let
                        expected =
                            Just ( x, xs )

                        actual =
                            x
                                :: xs
                                |> Queue.fromList
                                |> Queue.popFront
                                |> Maybe.map (\( y, ys ) -> ( y, Queue.toList ys ))
                    in
                    Expect.equal expected actual
            , fuzz (Fuzz.map2 (\a b -> ( a, b )) int (list int)) "popBack behaves" <|
                \( x, xs ) ->
                    let
                        expected =
                            Just ( x, xs )

                        actual =
                            List.append xs [ x ]
                                |> Queue.fromList
                                |> Queue.popBack
                                |> Maybe.map (\( y, ys ) -> ( y, Queue.toList ys ))
                    in
                    Expect.equal expected actual
            ]
        ]
