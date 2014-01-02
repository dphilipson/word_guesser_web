module Game.GameState where

import Import
import Game.Lexicon

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

loadGameState :: UserId -> YesodDB App GameState
loadGameState userId = do
    -- Assume just one game per user for now.
    gameEntity@(Entity gid _) <- loadGameEntity
    eLastGuess <- selectFirst [GuessGame ==. gid] [Desc GuessCount, LimitTo 1]
    let lastGuess = fmap entityVal eLastGuess
    return $ GameState gameEntity lastGuess
  where    
    loadGameEntity :: YesodDB App (Entity Game)
    loadGameEntity = do
        mGameEntity <- getBy $ UniqueGame userId
        case mGameEntity of
            Just entity -> return entity
            Nothing -> do
                lexicon <- lift getLexicon
                newSecret <- liftIO $ randomSecret lexicon
                let newGame = Game userId newSecret False
                newGid <- insert newGame
                return $ Entity newGid newGame 


