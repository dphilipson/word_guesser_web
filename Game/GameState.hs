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
