module Handler.Game where

import Import
import Game.GameState
import Game.Lexicon
import Text.Shakespeare.Text
import Yesod.Auth

getGameR :: Handler Html
getGameR = do
    mUserId <- maybeAuthId
    case mUserId of
        Just userId -> do
            gameState <- runDB $ loadGameState userId
            let latestMessage = gameMessage gameState
            defaultLayout $ do
                setTitle "Game - Word Guesser"
                $(widgetFile "game")
        Nothing -> redirect $ AuthR LoginR

loadGameState :: UserId -> YesodDB App GameState
loadGameState userId = do
    -- Assume just one game per user for now.
    gameEntity@(Entity gid _) <- loadGameEntity userId
    eLastGuess <- selectFirst [GuessGame ==. gid] [Desc GuessCount, LimitTo 1]
    let lastGuess = fmap entityVal eLastGuess
    return $ GameState gameEntity lastGuess
    
-- Creates the game if none exists for now.
loadGameEntity :: UserId -> YesodDB App (Entity Game)
loadGameEntity userId = do
    mGameEntity <- getBy $ UniqueGame userId
    case mGameEntity of
        Just entity -> return entity
        Nothing -> do
            lexicon <- lift getLexicon
            newSecret <- liftIO $ randomSecret lexicon
            let newGame = Game userId newSecret False
            newGid <- insert newGame
            return $ Entity newGid newGame 

guessForm :: Form Text
guessForm = renderDivs $ areq textField "Guess" Nothing

postGameR :: Handler Text
postGameR = do
    mUserId <- maybeAuthId
    case mUserId of
        Just userId -> do
            guessParams <- lookupPostParams "guess"
            case guessParams of
                rawWord : _ -> do
                    let word = canonicalize rawWord
                    lexicon <- getLexicon
                    if isValidGuess word lexicon
                        then do
                            newState <- runDB $ insertGuessAndGet userId word
                            return $ gameMessage newState
                        else return  
                            [st|I don't know the word "#{word}". Try again.|]
                _ -> return "Error: wrong POST params"
        Nothing -> return "Error: not logged in"

gameMessage :: GameState -> Text
gameMessage state
    | stateIsLost state  =
        [st|The secret was #{secret}. Better luck next time!|]
    | otherwise = case stateLatestWord state of
        Just w -> case compare secret w of
            LT -> [st|My word comes before "#{w}" in the dictionary.|]
            GT -> [st|My word comes after "#{w}" in the dictionary.|]
            EQ -> [st|You got it! The word was "#{secret}".
                      You used #{show $ stateGuessCount state} guesses.|]
        Nothing -> "I'm thinking of a word. Make a guess!"
  where secret = stateSecret state

insertGuessAndGet :: UserId -> Text -> YesodDB App GameState
insertGuessAndGet userId word = do
    gameState <- loadGameState userId
    let c = stateGuessCount gameState + 1
        gameId = entityKey $ stateGameEntity gameState
        newGuess = Guess gameId word c
    insert_ $ Guess gameId word c
    return gameState {stateLatestGuess = Just newGuess}
