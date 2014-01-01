module Handler.Game where

import Import
import Game.GameState
import Text.Shakespeare.Text

getGameR :: Handler Html
getGameR = do
    gameState <- runDB $ loadGameState
    let latestMessage = gameStateMessage gameState    
    (formWidget, formEnctype) <- generateFormPost guessForm
    defaultLayout $(widgetFile "game")

loadGameState :: YesodDB App GameState
loadGameState = do
    -- Assume just one game for now.
    gameEntity@(Entity gid _) <- loadGameEntity
    eLastGuess <- selectFirst [GuessGame ==. gid] [Desc GuessCount, LimitTo 1]
    let lastGuess = entityVal <$> eLastGuess
    return $ GameState gameEntity lastGuess
    
loadGameEntity :: YesodDB App (Entity Game)
loadGameEntity = do
    mGameEntity <- selectFirst ([] :: [Filter Game]) []
    case mGameEntity of
        Just entity -> return entity
        Nothing -> do
            -- For now, the words is always the same.
            let newGame = Game "swordfish"
            newGid <- insert newGame
            return $ Entity newGid newGame 

gameStateMessage :: GameState -> Text
gameStateMessage gameState = case stateLatestGuess gameState of
    Nothing -> "Make a guess!"
    Just Guess {guessWord = w, guessCount = c} ->
        case compare secret w of
            LT -> [st|My word comes before "#{w}" in the dictionary.|]
            GT -> [st|My word comes after "#{w}" in the dictionary.|]
            EQ -> [st|You got it! The word was "#{secret}".
                      You used #{show c} guesses.|]
          where secret = stateSecret gameState 

guessForm :: Form Text
guessForm = renderDivs $ areq textField "Guess" Nothing

postGameR :: Handler Html
postGameR = do
    ((result, _), _) <- runFormPost guessForm
    case result of
        FormSuccess word -> runDB $ updateWithGuess word
        _ -> return ()
    redirect GameR

updateWithGuess :: Text -> YesodDB App ()
updateWithGuess word = do
    gameState <- loadGameState
    let c = stateGuessCount gameState + 1
        Entity gameId _ = stateGameEntity gameState
    insert_ $ Guess gameId word c
