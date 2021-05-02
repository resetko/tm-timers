module PlayerQueue exposing (PlayerQueue, create, getCurrent, next, skipCurrent, toList)

import Player exposing (Player)
import Queue exposing (Queue)


type PlayerQueue
    = Q (Queue Player)


create : Player -> List Player -> PlayerQueue
create first others =
    Q ( first, others )


getCurrent : PlayerQueue -> Player
getCurrent q =
    case q of
        Q queue ->
            Queue.getCurrent queue


skipCurrent : PlayerQueue -> Maybe PlayerQueue
skipCurrent q =
    case q of
        Q queue ->
            if Queue.length queue == 1 then
                Nothing

            else
                Just (Q (Queue.removeCurrent queue))


next : PlayerQueue -> PlayerQueue
next q =
    case q of
        Q queue ->
            Q (Queue.next queue)


toList : PlayerQueue -> List Player
toList q =
    case q of
        Q queue ->
            Queue.toList queue
