module Queue exposing (Queue, createQueue, getCurrent, next, toList)

import List exposing (head, tail)


type alias Queue a =
    ( a, List a )


next : Queue a -> Queue a
next ( first, others ) =
    let
        nextItem =
            head others
    in
    case nextItem of
        Nothing ->
            ( first, others )

        Just newFirst ->
            let
                rest =
                    tail others
            in
            case rest of
                Nothing ->
                    ( first, others )

                Just justRest ->
                    ( newFirst, List.concat [ justRest, [ first ] ] )


getCurrent : Queue a -> a
getCurrent ( first, _ ) =
    first


createQueue : a -> List a -> Queue a
createQueue first rest =
    ( first, rest )


toList : Queue a -> List a
toList ( first, rest ) =
    first :: rest
