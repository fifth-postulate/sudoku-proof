module Stream.Queue exposing (Queue, append, empty, fromList, popBack, popFront, pushBack, pushFront, toList)

import Html exposing (a)


type Queue a
    = Queue { front : List a, back : List a }


empty : Queue a
empty =
    Queue { front = [], back = [] }


fromList : List a -> Queue a
fromList front =
    Queue { front = front, back = [] }


toList : Queue a -> List a
toList (Queue queue) =
    List.append queue.front <| List.reverse queue.back


pushFront : a -> Queue a -> Queue a
pushFront d (Queue queue) =
    Queue { queue | front = d :: queue.front }


popFront : Queue a -> Maybe ( a, Queue a )
popFront (Queue queue) =
    case queue.front of
        d :: ds ->
            Just ( d, Queue { queue | front = ds } )

        [] ->
            let
                front =
                    List.reverse queue.back
            in
            case front of
                d :: ds ->
                    Just ( d, Queue { queue | front = ds, back = [] } )

                [] ->
                    Nothing


pushBack : a -> Queue a -> Queue a
pushBack b (Queue queue) =
    Queue { queue | back = b :: queue.back }


popBack : Queue a -> Maybe ( a, Queue a )
popBack (Queue queue) =
    case queue.back of
        b :: bs ->
            Just ( b, Queue { queue | back = bs } )

        [] ->
            let
                back =
                    List.reverse queue.front
            in
            case back of
                b :: bs ->
                    Just ( b, Queue { queue | front = [], back = bs } )

                [] ->
                    Nothing


append : Queue a -> Queue a -> Queue a
append (Queue left) (Queue right) =
    Queue
        { front = List.concat [ left.front, List.reverse left.back ]
        , back = List.concat [ List.reverse right.front, right.back ]
        }
