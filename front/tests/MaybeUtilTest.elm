module MaybeUtilTest exposing (..)

import Expect
import Fuzz exposing (int)
import Maybe.Util exposing (orElse)
import Test exposing (..)


suite : Test
suite =
    describe "Maybe"
        [ describe "Util"
            [ describe "orElse"
                [ fuzz int "of Just and Just is first Just" <|
                    \subject ->
                        let
                            start =
                                Just subject

                            expected =
                                start
                        in
                        Expect.equal expected (start |> orElse (\_ -> Just <| subject + 1))
                , fuzz int "of Just and Nothing is Just" <|
                    \subject ->
                        let
                            start =
                                Just subject

                            expected =
                                start
                        in
                        Expect.equal expected (start |> orElse (\_ -> Nothing))
                , fuzz int "of Nothing and Just is Just" <|
                    \subject ->
                        let
                            start =
                                Nothing

                            expected =
                                Just subject
                        in
                        Expect.equal expected (start |> orElse (\_ -> Just subject))
                , test "of Nothing and Nothing is Nothing" <|
                    \_ ->
                        let
                            start =
                                Nothing

                            expected =
                                Nothing
                        in
                        Expect.equal expected (start |> orElse (\_ -> Nothing))
                ]
            ]
        ]
