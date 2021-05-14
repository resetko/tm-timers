module Player exposing (Player, allPlayersList, playerElementStyle, toString, view)

import Element exposing (Attribute, Element, el, height, px, rgb255, width)
import Element.Background as Background
import Element.Border as Border
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


playerElementStyle : Player -> List (Attribute msg)
playerElementStyle player =
    let
        common =
            [ Border.solid, Border.width 3 ]
    in
    case player of
        NoSkin ->
            [ Background.color <| rgb255 255 214 224
            , Border.color <| rgb255 255 91 102
            ]
                ++ common

        Blue ->
            [ Background.color <| rgb255 204 226 243
            , Border.color <| rgb255 0 110 197
            ]
                ++ common

        Green ->
            [ Background.color <| rgb255 220 239 221
            , Border.color <| rgb255 78 174 83
            ]
                ++ common

        Yellow ->
            [ Background.color <| rgb255 255 237 212
            , Border.color <| rgb255 255 167 38
            ]
                ++ common

        Black ->
            [ Background.color <| rgb255 222 224 227
            , Border.color <| rgb255 90 98 117
            ]
                ++ common


view : Player -> Element msg
view player =
    el
        ([ height <| px 50
         , width <| px 50
         , Font.size 14
         ]
            ++ playerElementStyle player
        )
        Element.none
