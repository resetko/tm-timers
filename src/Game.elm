module Game exposing (Game, gameView, newGame, nextPlayer, skipCurrentPlayer, startIteration, stopIteration, tick)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Player exposing (Player, playerView)
import PlayerQueue exposing (PlayerQueue)
import PlayerTimers exposing (PlayerTimers, decrementPlayerTick, getRemaining, newPlayerTimers)



-- import Queue exposing (Queue, getCurrent, length, next, removeCurrent, toList)


type Phase
    = Production { remainingTicks : Int }
    | Play { iterationQueue : PlayerQueue }


type alias Game =
    { iteration : Int
    , phase : Phase
    , playerTimers : PlayerTimers
    , players : PlayerQueue
    }


newGame : Player -> List Player -> Game
newGame first list =
    { iteration = 1
    , phase = Production { remainingTicks = 120 }
    , players = PlayerQueue.create first list
    , playerTimers = newPlayerTimers (first :: list) 2000
    }


startIteration : Game -> Game
startIteration game =
    case game.phase of
        Production _ ->
            { game | phase = Play { iterationQueue = game.players } }

        Play _ ->
            game


stopIteration : Game -> Game
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


nextPlayer : Game -> Game
nextPlayer game =
    case game.phase of
        Production _ ->
            game

        Play phaseInfo ->
            { game | phase = Play { phaseInfo | iterationQueue = PlayerQueue.next phaseInfo.iterationQueue } }


skipCurrentPlayer : Game -> Game
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


tick : Game -> Game
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


getIteration : Game -> Int
getIteration game =
    game.iteration


gameView : Game -> Html msg
gameView game =
    case game.phase of
        Production phase ->
            div []
                [ div
                    [ style "border" "1px solid red"
                    ]
                    [ text "production phase"
                    , div [] [ text (String.fromInt phase.remainingTicks) ]
                    ]
                , div [] [ text (String.fromInt (getIteration game)) ]
                , div [] (List.map playerView (PlayerQueue.toList game.players))
                ]

        Play phase ->
            let
                current =
                    getRemaining (PlayerQueue.getCurrent phase.iterationQueue) game.playerTimers
            in
            div []
                [ div [ style "border" "1px solid green" ] [ text "play phase" ]
                , div [] [ text ("player remaining: " ++ String.fromInt current) ]
                , div [] [ text (String.fromInt (getIteration game)) ]
                , div [] (List.map playerView (PlayerQueue.toList phase.iterationQueue))
                ]
