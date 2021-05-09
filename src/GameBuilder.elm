module GameBuilder exposing (ExternalMsg(..), Model, Msg, init, update, view)

import Element exposing (Element, column, row, spacing, text)
import Element.Input exposing (button)
import Html.Attributes exposing (selected)
import List exposing (filter, map)
import Player exposing (Player, allPlayersList)


type alias Model =
    List ( Player, Bool )


init : Model
init =
    map (\p -> ( p, False )) allPlayersList


getSelected : Model -> List Player
getSelected model =
    map (\( item, _ ) -> item) (filter (\( _, selected ) -> selected) model)


getRemaining : Model -> List Player
getRemaining model =
    map (\( item, _ ) -> item) (filter (\( _, selected ) -> not selected) model)



-- UPDATE


type Msg
    = Pick Player
    | Remove Player
    | StartClick ( Player, List Player )


type ExternalMsg
    = Start ( Player, List Player )


update : Msg -> Model -> ( Model, Maybe ExternalMsg )
update msg model =
    case msg of
        Pick player ->
            let
                filtered =
                    filter (\( item, _ ) -> item /= player) model
            in
            ( filtered ++ [ ( player, True ) ], Nothing )

        Remove player ->
            let
                filtered =
                    filter (\( item, _ ) -> item /= player) model
            in
            ( filtered ++ [ ( player, False ) ], Nothing )

        StartClick payload ->
            ( model, Just <| Start payload )



-- VIEW


playerButton : Maybe msg -> Player -> Element msg
playerButton msg player =
    button [] { label = Player.view player, onPress = msg }


viewStartGameButton : Model -> Element Msg
viewStartGameButton model =
    case getSelected model of
        [] ->
            Element.none

        x :: xs ->
            button [] { label = text "[Start game]", onPress = Just <| StartClick ( x, xs ) }


view : Model -> Element Msg
view model =
    column [ spacing 5 ]
        [ row [ spacing 5 ]
            (map
                (\item -> playerButton (Just (Pick item)) item)
                (getRemaining model)
            )
        , row [ spacing 5 ]
            (map
                (\item -> playerButton (Just (Remove item)) item)
                (getSelected model)
            )
        , viewStartGameButton model
        ]
