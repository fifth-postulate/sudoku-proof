module CombinatorTest exposing (..)

import Expect
import Sudoku exposing (Action, fill)
import Sudoku.Strategy exposing (Plan, Strategy)
import Sudoku.Strategy.Combinator exposing (either)
import Test exposing (..)


suite : Test
suite =
    describe "Sudoku"
        [ describe "Strategy"
            [ describe "Combinator"
                [ describe "either"
                    [ test "return first plan" <|
                        \_ ->
                            let
                                action =
                                    fill 0 1

                                strategy =
                                    either [ never, never, always ( action, 1 ) ]
                            in
                            Expect.equal (Just [ ( action, 1 ) ]) <| perform strategy
                    ]
                ]
            ]
        ]


never : Strategy
never _ =
    Nothing


always : ( Action, Int ) -> Strategy
always t _ =
    Just [ t ]


perform : Strategy -> Maybe Plan
perform strategy =
    strategy (Sudoku.emptySudoku 4)
