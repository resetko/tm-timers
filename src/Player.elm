module Player exposing (Player, getPlayerCssStyle, newBlack, newBlue, newRed, playerLabel, playerView, toString)

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
