module Stream exposing (Stream, afterwards, empty, fromList, head, map, singleton)

import Stream.Queue as Queue exposing (Queue)


type Stream a
    = Stream { values : List a, promises : Queue (() -> Stream a) }


empty : Stream a
empty =
    Stream { values = [], promises = Queue.empty }


singleton : a -> Stream a
singleton value =
    Stream { values = [ value ], promises = Queue.empty }


fromList : List a -> Stream a
fromList values =
    Stream { values = values, promises = Queue.empty }


head : Stream a -> Maybe ( a, Stream a )
head (Stream stream) =
    case stream.values of
        v :: vs ->
            Just ( v, Stream { stream | values = vs } )

        [] ->
            let
                adjoin ( promise, promises ) =
                    promise ()
                        |> append (Stream { stream | promises = promises })
            in
            stream.promises
                |> Queue.popFront
                |> Maybe.map adjoin
                |> Maybe.andThen head


afterwards : (() -> Stream a) -> Stream a -> Stream a
afterwards promise (Stream stream) =
    Stream { stream | promises = Queue.pushBack promise stream.promises }


append : Stream a -> Stream a -> Stream a
append (Stream left) (Stream right) =
    Stream { values = List.append left.values right.values, promises = Queue.append left.promises right.promises }


map : (a -> b) -> Stream a -> Stream b
map f (Stream stream) =
    let
        lift promise =
            map f << promise
    in
    Stream { values = List.map f stream.values, promises = Queue.map lift stream.promises }