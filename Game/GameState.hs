module Game.GameState where

import Import

data GameState = GameState { stateGameEntity :: Entity Game
                           , stateLatestGuess :: Maybe Guess
                           }

stateGameId :: GameState -> GameId
stateGameId = entityKey . stateGameEntity

stateSecret :: GameState -> Text
stateSecret = gameSecret . entityVal . stateGameEntity

stateGuessCount :: GameState -> Int
stateGuessCount = (maybe 0 guessCount) . stateLatestGuess

stateLatestWord :: GameState -> Maybe Text
stateLatestWord = (guessWord <$>) . stateLatestGuess

stateIsWon :: GameState -> Bool
stateIsWon state = stateLatestWord state == Just (stateSecret state)

stateIsLost :: GameState -> Bool
stateIsLost = gameSurrendered . entityVal . stateGameEntity

stateIsComplete :: GameState -> Bool
stateIsComplete state = stateIsWon state || stateIsLost state
