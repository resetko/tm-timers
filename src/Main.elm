module Main exposing (..)

import Browser exposing (Document)
import Element exposing (Element, column, row, spacing, text)
import Element.Input exposing (button)
import Game exposing (Game, gameView, newGame, nextPlayer, skipCurrentPlayer, startIteration, tick)
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


type alias Model =
    { timerState : TimerState
    , game : Game
    , builder : GameBuilder.Model
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Stopped (newGame newBlue [ newRed, newBlack ]) GameBuilder.init, Cmd.none )


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
    | NextPlayer
    | StartIteration
    | StopTimer
    | SkipCurrent
    | BuilderMsg GameBuilder.Msg


startTimer : Model -> Model
startTimer model =
    { model | timerState = Running }


stopTimer : Model -> Model
stopTimer model =
    { model | timerState = Stopped }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick ->
            ( { model | game = tick model.game }, Cmd.none )

        StartTimer ->
            ( startTimer model, Cmd.none )

        StopTimer ->
            ( stopTimer model, Cmd.none )

        StartIteration ->
            ( { model | game = startIteration model.game }, Cmd.none )

        NextPlayer ->
            ( { model | game = nextPlayer model.game }, Cmd.none )

        SkipCurrent ->
            ( { model | game = skipCurrentPlayer model.game }, Cmd.none )

        BuilderMsg _ ->
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
        , button [] { onPress = Just NextPlayer, label = text "next player" }
        , button [] { onPress = Just StartIteration, label = text "start iteration" }
        , button [] { onPress = Just SkipCurrent, label = text "skip current" }
        ]


view : Model -> Document Msg
view model =
    { title = "Terraforming mars clock"
    , body =
        [ Element.layout []
            (column
                []
                [ timerStateView model.timerState
                , viewControls
                , gameView model.game
                , Element.map (\msg -> BuilderMsg msg) (GameBuilder.view model.builder)
                ]
            )
        ]
    }
