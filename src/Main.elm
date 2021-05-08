module Main exposing (..)

import Browser exposing (Document)
import Element exposing (Element, column, row, spacing, text)
import Element.Input exposing (button)
import Game
import GameBuilder
import Player exposing (newBlack, newBlue, newRed)
import Time


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type TimerState
    = Running
    | Stopped


type AppState
    = Builder GameBuilder.Model
    | Game Game.Model


type alias Model =
    { timerState : TimerState
    , app : AppState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Stopped (Builder GameBuilder.init), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        timer =
            model.timerState
    in
    case timer of
        Running ->
            Time.every 1000 createTick

        Stopped ->
            Sub.none


createTick : any -> Msg
createTick _ =
    Tick


type Msg
    = Tick
    | StartTimer
    | StopTimer
    | GameMsg Game.Msg
    | BuilderMsg GameBuilder.Msg


startTimer : Model -> Model
startTimer model =
    { model | timerState = Running }


stopTimer : Model -> Model
stopTimer model =
    { model | timerState = Stopped }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.app of
        Builder builder ->
            case msg of
                BuilderMsg builderMsg ->
                    ( { model | app = Builder (GameBuilder.update builderMsg builder) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Game game ->
            case msg of
                Tick ->
                    ( { model | app = Game (Game.update Game.onTick game) }, Cmd.none )

                GameMsg gameMsg ->
                    ( { model | app = Game (Game.update gameMsg game) }, Cmd.none )

                StartTimer ->
                    ( startTimer model, Cmd.none )

                StopTimer ->
                    ( stopTimer model, Cmd.none )

                _ ->
                    ( model, Cmd.none )


timerStateLabel : TimerState -> String
timerStateLabel state =
    case state of
        Running ->
            "running"

        Stopped ->
            "stopped"



-- VIEW


timerStateView : TimerState -> Element msg
timerStateView state =
    text <| timerStateLabel state


viewControls : Element Msg
viewControls =
    row [ spacing 15 ]
        [ button [] { onPress = Just StartTimer, label = text "start increment timer" }
        , button [] { onPress = Just StopTimer, label = text "stop increment timer" }
        ]


viewApp : Model -> Element Msg
viewApp model =
    case model.app of
        Game game ->
            column
                []
                [ timerStateView model.timerState
                , viewControls
                , Game.view game
                ]

        Builder builder ->
            Element.map (\msg -> BuilderMsg msg) (GameBuilder.view builder)


view : Model -> Document Msg
view model =
    { title = "Terraforming mars clock"
    , body = [ Element.layout [] (viewApp model) ]
    }
