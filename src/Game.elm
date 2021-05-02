module Game exposing (Game, gameView, newGame, nextPlayer, skipCurrentPlayer, startIteration, stopIteration, tick)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Player exposing (Player, PlayerTimers, decrementPlayerTick, getRemaining, newPlayerTimers, playerView)
import Queue exposing (Queue, getCurrent, length, next, removeCurrent, toList)


type Phase
    = Production { remainingTicks : Int }
    | Play { iterationQueue : Queue Player }


type alias Game =
    { iteration : Int
    , phase : Phase
    , playerTimers : PlayerTimers
    , players : Queue Player
    }


newGame : Player -> List Player -> Game
newGame first list =
    { iteration = 1
    , phase = Production { remainingTicks = 120 }
    , players = ( first, list )
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
                , players = next game.players
            }


nextPlayer : Game -> Game
nextPlayer game =
    case game.phase of
        Production _ ->
            game

        Play phaseInfo ->
            { game | phase = Play { phaseInfo | iterationQueue = next phaseInfo.iterationQueue } }


skipCurrentPlayer : Game -> Game
skipCurrentPlayer game =
    case game.phase of
        Production _ ->
            game

        Play phase ->
            if length phase.iterationQueue == 1 then
                stopIteration game

            else
                { game | phase = Play { iterationQueue = removeCurrent phase.iterationQueue } }


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
                    getCurrent phase.iterationQueue
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
                , div [] (List.map playerView (toList game.players))
                ]

        Play phase ->
            let
                current =
                    getRemaining (getCurrent phase.iterationQueue) game.playerTimers
            in
            div []
                [ div [ style "border" "1px solid green" ] [ text "play phase" ]
                , div [] [ text ("player remaining: " ++ String.fromInt current) ]
                , div [] [ text (String.fromInt (getIteration game)) ]
                , div [] (List.map playerView (toList phase.iterationQueue))
                ]
