module Queue exposing (Queue, createQueue, getCurrent, length, next, removeCurrent, toList)

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


removeCurrent : Queue a -> Queue a
removeCurrent ( first, others ) =
    let
        nextItem =
            head others

        noChange =
            ( first, others )
    in
    case nextItem of
        Nothing ->
            noChange

        Just newFirst ->
            let
                rest =
                    tail others
            in
            case rest of
                Nothing ->
                    noChange

                Just justRest ->
                    ( newFirst, justRest )


length : Queue a -> Int
length ( _, rest ) =
    List.length rest + 1


getCurrent : Queue a -> a
getCurrent ( first, _ ) =
    first


createQueue : a -> List a -> Queue a
createQueue first rest =
    ( first, rest )


toList : Queue a -> List a
toList ( first, rest ) =
    first :: rest
