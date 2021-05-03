module GameBuilder exposing (Model, Msg, init, update, view)

import Element exposing (Element, column, row, spacing)
import List exposing (filter, map)
import Player exposing (Player, allPlayersList)


type alias Model =
    List ( Player, Bool )


init : Model
init =
    map (\p -> ( p, False )) allPlayersList


type Msg
    = Pick Player
    | Remove Player


update : Msg -> Model -> Model
update msg model =
    case msg of
        Pick player ->
            let
                filtered =
                    filter (\( item, _ ) -> item == player) model
            in
            filtered ++ [ ( player, True ) ]

        Remove player ->
            let
                filtered =
                    filter (\( item, _ ) -> item == player) model
            in
            filtered ++ [ ( player, False ) ]


getSelected : Model -> List Player
getSelected model =
    map (\( item, _ ) -> item) (filter (\( _, selected ) -> selected) model)


getRemaining : Model -> List Player
getRemaining model =
    map (\( item, _ ) -> item) (filter (\( _, selected ) -> not selected) model)


view : Model -> Element msg
view model =
    column [ spacing 5 ]
        [ row [ spacing 5 ]
            (map
                Player.view
                (getRemaining model)
            )
        , row [ spacing 5 ]
            (map
                Player.view
                (getSelected model)
            )
        ]
