module PlayerTimers exposing (PlayerTimers, decrement, getRemaining, init)

import Dict exposing (Dict, get, update)
import Player exposing (Player, toString)


type PlayerTimers
    = Table (Dict String Int)


init : List Player -> PlayerTimers
init players =
    let
        ticks =
            round <| 180 * 60 / (toFloat <| List.length players)
    in
    Table
        (Dict.fromList (List.map (\item -> ( toString item, ticks )) players))


decrement : Player -> PlayerTimers -> PlayerTimers
decrement player table =
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
