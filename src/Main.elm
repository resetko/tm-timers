module Main exposing (..)

import App
import Browser exposing (Document)
import Element exposing (column)
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
    { app : App.Model
    , date : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { app = App.init, date = "" }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 (\_ -> Tick)


type Msg
    = Tick
    | AppMsg App.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( model, Cmd.none )

        AppMsg appMsg ->
            ( { model | app = App.update appMsg model.app }, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Terraforming mars clock"
    , body =
        [ Element.layout [] <|
            column []
                [ Element.map (\msg -> AppMsg msg) (App.view model.app)
                ]
        ]
    }
