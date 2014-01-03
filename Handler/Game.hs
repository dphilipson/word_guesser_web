module Handler.Game where

import Import
import Api.StatusResponse
import Game.GameState
import Game.Lexicon
import Text.Shakespeare.Text
import Util.Message
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
        Nothing -> do
            setMessageWarning "You must log in to play."
            redirect $ AuthR LoginR

guessForm :: Form Text
guessForm = renderDivs $ areq textField "Guess" Nothing

postGameR :: Handler Value
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
                            return $ toJSON $ fromGameState newState
                        else do
                            state <- runDB $ loadGameState userId
                            return $ unknownWordResponse state word
                _ -> return Null
        Nothing -> return Null
  where
    unknownWordResponse state word = toJSON
        $ StatusResponse [st|I don't know the word "#{word}". Try again.|]
        $ statusEnum state

insertGuessAndGet :: UserId -> Text -> YesodDB App GameState
insertGuessAndGet userId word = do
    gameState <- loadGameState userId
    let c = stateGuessCount gameState + 1
        gameId = entityKey $ stateGameEntity gameState
        newGuess = Guess gameId word c
    insert_ $ Guess gameId word c
    return gameState {stateLatestGuess = Just newGuess}
