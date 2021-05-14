module Main exposing (..)

import App
import Browser exposing (Document)
import DateFormat
import Element exposing (Element, column, el, fill, height, padding, spacing, text)
import Task
import Time exposing (Posix, Zone, millisToPosix, utc)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { app : App.Model
    , time : ( Zone, Posix )
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        adjustTask =
            Task.map2 (\zone time -> ( zone, time )) Time.here Time.now
    in
    ( { app = App.init, time = ( utc, millisToPosix 0 ) }
    , Task.perform AdjustTime adjustTask
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 100 (\time -> Tick time)


type Msg
    = Tick Posix
    | AdjustTime ( Zone, Posix )
    | AppMsg App.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( zone, _ ) =
            model.time
    in
    case msg of
        AdjustTime timeInfo ->
            ( { model | time = timeInfo }, Cmd.none )

        Tick time ->
            ( { model | app = App.update App.onTick model.app, time = ( zone, time ) }, Cmd.none )

        AppMsg appMsg ->
            ( { model | app = App.update appMsg model.app }, Cmd.none )



-- VIEW


viewTime : Model -> Element msg
viewTime { time } =
    let
        ( zone, posix ) =
            time
    in
    text <|
        DateFormat.format
            [ DateFormat.dayOfMonthFixed
            , DateFormat.text "-"
            , DateFormat.monthFixed
            , DateFormat.text "-"
            , DateFormat.yearNumberLastTwo
            , DateFormat.text " "
            , DateFormat.hourMilitaryFixed
            , DateFormat.text ":"
            , DateFormat.minuteFixed
            , DateFormat.text ":"
            , DateFormat.secondFixed
            ]
            zone
            posix


view : Model -> Document Msg
view model =
    { title = "Terraforming mars clock"
    , body =
        [ Element.layout [ height fill ] <|
            column [ padding 15, spacing 15, height fill ]
                [ el [ height fill ] (Element.map (\msg -> AppMsg msg) (App.view model.app))
                , el [] <| viewTime model
                ]
        ]
    }
