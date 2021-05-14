module Game exposing (Model, Msg, init, onTick, update, view)

import DateFormat
import Element exposing (Attribute, Element, column, row, spacing, text)
import Player exposing (Player)
import PlayerQueue exposing (PlayerQueue)
import PlayerTimers exposing (PlayerTimers, decrement, getRemaining)
import Time exposing (millisToPosix, utc)
import UIKit exposing (button, iconButton)



-- Constants


initialProductionTime : Int
initialProductionTime =
    60 * 7


productionTime : Int
productionTime =
    60 * 4



-- Model


type Phase
    = Production { remainingTicks : Int }
    | Play { iterationQueue : PlayerQueue }


type TimerState
    = Running
    | Stopped


type alias Model =
    { iteration : Int
    , phase : Phase
    , playerTimers : PlayerTimers
    , players : PlayerQueue
    , timer : TimerState
    }


init : Player -> List Player -> Model
init first list =
    { iteration = 1
    , phase = Production { remainingTicks = initialProductionTime }
    , players = PlayerQueue.create first list
    , playerTimers = PlayerTimers.init (first :: list)
    , timer = Running
    }



-- UPDATE


type Msg
    = Tick
    | StartTimer
    | StopTimer
    | StartIteration
    | SkipCurrentPlayer
    | NextPlayer


onTick : Msg
onTick =
    Tick


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick ->
            tick model

        StartIteration ->
            startIteration model

        SkipCurrentPlayer ->
            skipCurrentPlayer model

        NextPlayer ->
            nextPlayer model

        StartTimer ->
            { model | timer = Running }

        StopTimer ->
            { model | timer = Stopped }


viewTimerButton : List (Attribute Msg) -> TimerState -> Element Msg -> Element Msg
viewTimerButton attrs state label =
    case state of
        Running ->
            iconButton
                attrs
                { icon = text "||", label = label, onPress = Just StopTimer }

        Stopped ->
            iconButton
                attrs
                { icon = text ">", label = label, onPress = Just StartTimer }


viewMainButton : Model -> Element Msg
viewMainButton model =
    case model.phase of
        Production { remainingTicks } ->
            viewTimerButton [] model.timer (viewTicksRemaining remainingTicks)

        Play phase ->
            let
                current =
                    PlayerQueue.getCurrent phase.iterationQueue

                currentRemaining =
                    getRemaining current model.playerTimers
            in
            viewTimerButton (Player.playerElementAttributes current) model.timer (viewTicksRemaining currentRemaining)


viewTicksRemaining : Int -> Element msg
viewTicksRemaining ticks =
    let
        time =
            DateFormat.format
                [ DateFormat.hourMilitaryFixed
                , DateFormat.text ":"
                , DateFormat.minuteFixed
                , DateFormat.text ":"
                , DateFormat.secondFixed
                ]
                utc
                (millisToPosix <| abs <| ticks * 1000)
    in
    -- TODO Show timer in red if negative
    if ticks >= 0 then
        text <| "+" ++ time

    else
        text <| "-" ++ time


view : Model -> Element Msg
view model =
    case model.phase of
        Production _ ->
            let
                playerList =
                    PlayerQueue.toList model.players
            in
            column [ spacing 5 ]
                [ text <| "Iteration: " ++ String.fromInt (getIteration model)
                , viewMainButton model
                , button [] { label = text "GO", onPress = Just StartIteration }
                , row [ spacing 5 ] (List.map Player.view playerList)
                ]

        Play phase ->
            column [ spacing 5 ]
                [ text <| "Iteration: " ++ String.fromInt (getIteration model)
                , viewMainButton model
                , row [ spacing 15 ]
                    -- TODO => button must be inactive if only one player is remaining
                    [ button [] { label = text "=>", onPress = Just NextPlayer }
                    , button [] { label = text "X", onPress = Just SkipCurrentPlayer }
                    ]
                , row [ spacing 5 ] (List.map Player.view <| PlayerQueue.getNextList phase.iterationQueue)
                ]


startIteration : Model -> Model
startIteration game =
    case game.phase of
        Production _ ->
            { game | phase = Play { iterationQueue = game.players } }

        Play _ ->
            game


stopIteration : Model -> Model
stopIteration game =
    case game.phase of
        Production _ ->
            game

        Play _ ->
            { game
                | phase = Production { remainingTicks = productionTime }
                , iteration = game.iteration + 1
                , players = PlayerQueue.next game.players
            }


nextPlayer : Model -> Model
nextPlayer game =
    case game.phase of
        Production _ ->
            game

        Play phase ->
            { game | phase = Play { phase | iterationQueue = PlayerQueue.next phase.iterationQueue } }


skipCurrentPlayer : Model -> Model
skipCurrentPlayer game =
    case game.phase of
        Production _ ->
            game

        Play phase ->
            case PlayerQueue.skipCurrent phase.iterationQueue of
                Nothing ->
                    stopIteration game

                Just newQueue ->
                    { game | phase = Play { iterationQueue = newQueue } }


tick : Model -> Model
tick game =
    case game.timer of
        Stopped ->
            game

        Running ->
            case game.phase of
                Production phase ->
                    { game
                        | phase = Production { phase | remainingTicks = phase.remainingTicks - 1 }
                    }

                Play phase ->
                    let
                        current =
                            PlayerQueue.getCurrent phase.iterationQueue
                    in
                    { game | playerTimers = decrement current game.playerTimers }


getIteration : Model -> Int
getIteration game =
    game.iteration
