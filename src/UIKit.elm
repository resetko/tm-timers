module UIKit exposing (button, buttonBase, iconButton)

import Element exposing (Attr, Attribute, Color, Element, focused, padding, rgb255, row, spacing)
import Element.Border as Border
import Element.Font as Font
import Element.Input


noShadow : Attr decorative msg
noShadow =
    Border.shadow { size = 0, blur = 0, offset = ( 0, 0 ), color = rgb255 255 255 255 }


teal : Color
teal =
    rgb255 26 178 168


buttonBase : List (Attribute msg) -> { onPress : Maybe msg, label : Element msg } -> Element msg
buttonBase attrs params =
    Element.Input.button
        (focused [ noShadow ] :: attrs)
        params


button : List (Attribute msg) -> { onPress : Maybe msg, label : Element msg } -> Element msg
button attrs params =
    buttonBase
        ([ Border.color teal
         , Font.color teal
         , Border.width 3
         , padding 15
         ]
            ++ attrs
        )
        params


iconButton : List (Attribute msg) -> { onPress : Maybe msg, label : Element msg, icon : Element msg } -> Element msg
iconButton attrs { onPress, icon, label } =
    button
        ([ Border.color teal
         , Font.color teal
         , Border.width 3
         , padding 15
         ]
            ++ attrs
        )
        { onPress = onPress, label = row [ spacing 5 ] [ label, icon ] }
