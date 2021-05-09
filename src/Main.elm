module Main exposing (..)

import Browser exposing (Document)
import Element exposing (Element)
import Game
import GameBuilder
import Time


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type AppState
    = Builder GameBuilder.Model
    | Game Game.Model


type alias Model =
    { app : AppState
    , date : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { app = Builder GameBuilder.init, date = "" }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 (\_ -> Tick)


type Msg
    = Tick
    | GameMsg Game.Msg
    | BuilderMsg GameBuilder.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.app of
        Builder builder ->
            case msg of
                BuilderMsg builderMsg ->
                    let
                        ( newBuilder, maybeBuilderExternalMsg ) =
                            GameBuilder.update builderMsg builder
                    in
                    case maybeBuilderExternalMsg of
                        Nothing ->
                            ( { model | app = Builder newBuilder }, Cmd.none )

                        Just builderExternalMsg ->
                            case builderExternalMsg of
                                GameBuilder.Start ( first, rest ) ->
                                    ( { model | app = Game <| Game.init first rest }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Game game ->
            case msg of
                Tick ->
                    ( { model | app = Game (Game.update Game.onTick game) }, Cmd.none )

                GameMsg gameMsg ->
                    ( { model | app = Game (Game.update gameMsg game) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- VIEW


viewApp : Model -> Element Msg
viewApp model =
    case model.app of
        Game game ->
            Element.map (\msg -> GameMsg msg) (Game.view game)

        Builder builder ->
            Element.map (\msg -> BuilderMsg msg) (GameBuilder.view builder)


view : Model -> Document Msg
view model =
    { title = "Terraforming mars clock"
    , body = [ Element.layout [] (viewApp model) ]
    }
