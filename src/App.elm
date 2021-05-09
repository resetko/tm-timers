module App exposing (Model, Msg, init, update, view)

import Element exposing (Element)
import Game
import GameBuilder


type Model
    = Builder GameBuilder.Model
    | Game Game.Model


init : Model
init =
    Builder GameBuilder.init



-- UPDATE


type Msg
    = Tick
    | GameMsg Game.Msg
    | BuilderMsg GameBuilder.Msg


update : Msg -> Model -> Model
update msg model =
    case model of
        Builder builder ->
            case msg of
                BuilderMsg builderMsg ->
                    let
                        ( newBuilder, maybeBuilderExternalMsg ) =
                            GameBuilder.update builderMsg builder
                    in
                    case maybeBuilderExternalMsg of
                        Nothing ->
                            Builder newBuilder

                        Just builderExternalMsg ->
                            case builderExternalMsg of
                                GameBuilder.Start ( first, rest ) ->
                                    Game <| Game.init first rest

                GameMsg _ ->
                    model

                Tick ->
                    model

        Game game ->
            case msg of
                Tick ->
                    Game (Game.update Game.onTick game)

                GameMsg gameMsg ->
                    Game (Game.update gameMsg game)

                BuilderMsg _ ->
                    model



-- VIEW


view : Model -> Element Msg
view model =
    case model of
        Game game ->
            Element.map (\msg -> GameMsg msg) (Game.view game)

        Builder builder ->
            Element.map (\msg -> BuilderMsg msg) (GameBuilder.view builder)
