module Player exposing (Player, PlayerTimers, decrementPlayerTick, getPlayerCssStyle, getRemaining, newBlack, newBlue, newPlayerTimers, newRed, playerLabel, playerView)

import Dict exposing (Dict, get, update)
import Html exposing (Attribute, Html, div, text)
import Html.Attributes exposing (style)


type Player
    = Blue
    | Yellow
    | Green
    | Black
    | NoSkin


toString : Player -> String
toString player =
    case player of
        NoSkin ->
            "noskin"

        Blue ->
            "blue"

        Green ->
            "green"

        Yellow ->
            "yellow"

        Black ->
            "black"


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


newBlue : Player
newBlue =
    Blue


newBlack : Player
newBlack =
    Black


newRed : Player
newRed =
    NoSkin


playerLabel : Player -> String
playerLabel player =
    case player of
        NoSkin ->
            "NoSkin"

        Blue ->
            "Blue"

        Green ->
            "Green"

        Yellow ->
            "Yellow"

        Black ->
            "Black"


getPlayerCssStyle : Player -> Attribute msg
getPlayerCssStyle player =
    case player of
        NoSkin ->
            style "background" "red"

        Blue ->
            style "background" "blue"

        Green ->
            style "background" "green"

        Yellow ->
            style "background" "yellow"

        Black ->
            style "background" "grey"


playerView : Player -> Html msg
playerView player =
    div [ style "height" "50px", style "width" "50px", getPlayerCssStyle player ] [ text (playerLabel player) ]
