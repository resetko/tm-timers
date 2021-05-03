module Game exposing (Game, gameView, newGame, nextPlayer, skipCurrentPlayer, startIteration, tick)

import Element exposing (Element, column, rgb255, row, spacing, text)
import Element.Border as Border
import Player exposing (Player)
import PlayerQueue exposing (PlayerQueue)
import PlayerTimers exposing (PlayerTimers, decrementPlayerTick, getRemaining, newPlayerTimers)


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


gameView : Game -> Element msg
gameView game =
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
                , text ("player remaining: " ++ String.fromInt current)
                , row [] (List.map Player.view playerList)
                ]
