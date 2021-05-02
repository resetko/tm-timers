module PlayerTimers exposing (PlayerTimers, decrementPlayerTick, getRemaining, newPlayerTimers)

import Dict exposing (Dict, get, update)
import Player exposing (Player, toString)


type PlayerTimers
    = Table (Dict String Int)


newPlayerTimers : List Player -> Int -> PlayerTimers
newPlayerTimers players ticks =
    Table
        (Dict.fromList (List.map (\item -> ( toString item, ticks )) players))


decrementPlayerTick : Player -> PlayerTimers -> PlayerTimers
decrementPlayerTick player table =
    case table of
        Table dict ->
            let
                newDict =
                    update (toString player) (Maybe.map (\ticks -> ticks - 1)) dict
            in
            Table newDict


getRemaining : Player -> PlayerTimers -> Int
getRemaining player timers =
    case timers of
        Table dict ->
            case get (toString player) dict of
                Nothing ->
                    0

                Just ticks ->
                    ticks
