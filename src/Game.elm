module Game exposing (Model, Msg, init, onTick, update, view)

import Element exposing (Element, column, rgb255, row, spacing, text)
import Element.Border as Border
import Element.Input exposing (button)
import Player exposing (Player)
import PlayerQueue exposing (PlayerQueue)
import PlayerTimers exposing (PlayerTimers, decrement, getRemaining)


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
    , phase = Production { remainingTicks = 180 }
    , players = PlayerQueue.create first list
    , playerTimers = PlayerTimers.init (first :: list)
    , timer = Stopped
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


viewTimer : TimerState -> Element Msg
viewTimer state =
    case state of
        Running ->
            button [] { onPress = Just StopTimer, label = text "stop increment timer" }

        Stopped ->
            button [] { onPress = Just StartTimer, label = text "start increment timer" }


viewControls : Model -> Element Msg
viewControls model =
    case model.phase of
        Production _ ->
            row [ spacing 15 ]
                [ viewTimer model.timer
                , button [] { label = text "[Start iteration]", onPress = Just StartIteration }
                ]

        Play _ ->
            row [ spacing 15 ]
                [ viewTimer model.timer
                , button [] { label = text "[Next player]", onPress = Just NextPlayer }
                , button [] { label = text "[Skip current player]", onPress = Just SkipCurrentPlayer }
                ]


view : Model -> Element Msg
view model =
    case model.phase of
        Production phase ->
            let
                playerList =
                    PlayerQueue.toList model.players
            in
            column [ spacing 5 ]
                [ viewControls model
                , row
                    [ Border.color <| rgb255 255 0 0, spacing 15 ]
                    [ text "production phase", text (String.fromInt phase.remainingTicks) ]
                , text (String.fromInt (getIteration model))
                , row [] (List.map Player.view playerList)
                ]

        Play phase ->
            let
                current =
                    getRemaining (PlayerQueue.getCurrent phase.iterationQueue) model.playerTimers

                playerList =
                    PlayerQueue.toList phase.iterationQueue
            in
            column [ spacing 5 ]
                [ viewControls model
                , row
                    [ Border.color <| rgb255 0 255 0, spacing 15 ]
                    [ text "play phase" ]
                , text (String.fromInt (getIteration model))
                , text ("player remaining: " ++ String.fromInt current)
                , row [] (List.map Player.view playerList)
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
                | phase = Production { remainingTicks = 120 }
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
