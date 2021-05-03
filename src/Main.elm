module Main exposing (..)

import Browser exposing (Document)
import Element
import Game exposing (Game, gameView, newGame, nextPlayer, skipCurrentPlayer, startIteration, tick)
import GameBuilder
import Html exposing (Html, button, div, hr, text)
import Html.Events exposing (onClick)
import Player exposing (newBlack, newBlue, newRed)
import Time



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


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



-- UPDATE


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


timerStateView : TimerState -> Html msg
timerStateView state =
    div [] [ text (timerStateLabel state) ]


view : Model -> Document Msg
view model =
    { title = "Terraforming mars clock"
    , body =
        [ div
            []
            [ timerStateView model.timerState
            , button [ onClick StartTimer ] [ text "start increment timer" ]
            , button [ onClick StopTimer ] [ text "stop increment timer" ]
            , button [ onClick NextPlayer ] [ text "next player" ]
            , button [ onClick StartIteration ] [ text "start iteration" ]
            , button [ onClick SkipCurrent ] [ text "skip current" ]
            , gameView model.game
            , hr [] []
            , Element.layout [] (Element.map (\msg -> BuilderMsg msg) (GameBuilder.view model.builder))
            ]
        ]
    }
