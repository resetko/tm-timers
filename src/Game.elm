module Game exposing (Model, Msg, init, onTick, update, view)

import Element exposing (Element, column, rgb255, row, spacing, text)
import Element.Border as Border
import Player exposing (Player)
import PlayerQueue exposing (PlayerQueue)
import PlayerTimers exposing (PlayerTimers, decrementPlayerTick, getRemaining, newPlayerTimers)


type Phase
    = Production { remainingTicks : Int }
    | Play { iterationQueue : PlayerQueue }


type alias Model =
    { iteration : Int
    , phase : Phase
    , playerTimers : PlayerTimers
    , players : PlayerQueue
    }


init : Player -> List Player -> Model
init first list =
    { iteration = 1
    , phase = Production { remainingTicks = 120 }
    , players = PlayerQueue.create first list
    , playerTimers = newPlayerTimers (first :: list) 2000
    }


type Msg
    = Tick
    | StartIteration
    | StopIteration
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

        StopIteration ->
            stopIteration model

        SkipCurrentPlayer ->
            skipCurrentPlayer model

        NextPlayer ->
            nextPlayer model


view : Model -> Element msg
view game =
    case game.phase of
        Production phase ->
            let
                playerList =
                    PlayerQueue.toList game.players
            in
            column [ spacing 5 ]
                [ row
                    [ Border.color <| rgb255 255 0 0, spacing 15 ]
                    [ text "production phase", text (String.fromInt phase.remainingTicks) ]
                , text (String.fromInt (getIteration game))
                , row [] []
                , row [] (List.map Player.view playerList)
                ]

        Play phase ->
            let
                current =
                    getRemaining (PlayerQueue.getCurrent phase.iterationQueue) game.playerTimers

                playerList =
                    PlayerQueue.toList phase.iterationQueue
            in
            column [ spacing 5 ]
                [ row
                    [ Border.color <| rgb255 0 255 0, spacing 15 ]
                    [ text "play phase" ]
                , text (String.fromInt (getIteration game))
                , row [] []
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

        Play phaseInfo ->
            { game | phase = Play { phaseInfo | iterationQueue = PlayerQueue.next phaseInfo.iterationQueue } }


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
            { game | playerTimers = decrementPlayerTick current game.playerTimers }


getIteration : Model -> Int
getIteration game =
    game.iteration
