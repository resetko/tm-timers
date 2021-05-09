module Player exposing (Player, allPlayersList, playerBackground, playerLabel, toString, view)

import Element exposing (Attribute, Element, el, height, px, rgb255, text, width)
import Element.Background as Background
import Element.Font as Font


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


allPlayersList : List Player
allPlayersList =
    [ Green, Yellow, Blue, Black, NoSkin ]


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


playerBackground : Player -> Attribute msg
playerBackground player =
    case player of
        NoSkin ->
            Background.color <| rgb255 255 0 0

        Blue ->
            Background.color <| rgb255 0 0 255

        Green ->
            Background.color <| rgb255 0 255 0

        Yellow ->
            Background.color <| rgb255 255 255 0

        Black ->
            Background.color <| rgb255 128 128 128


view : Player -> Element msg
view player =
    el
        [ height <| px 50
        , width <| px 50
        , Font.size 14
        , playerBackground player
        ]
        (text <| playerLabel player)
