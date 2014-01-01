module Handler.Game where

import Import
import Text.Shakespeare.Text

data GameState = GameState { game :: Game
                           , latestGuess :: Maybe Guess
                           }

getGameR :: Handler Html
getGameR = do
    gameState <- runDB $ loadGameState
    let latestMessage = gameStateMessage gameState    
    (formWidget, formEnctype) <- generateFormPost guessForm
    defaultLayout $(widgetFile "game")

loadGameState :: YesodDB App GameState
loadGameState = do
    -- Assume just one game for now.
    Entity gid game_ <- loadGameEntity
    eLastGuess <- selectFirst [GuessGame ==. gid] [Desc GuessCount, LimitTo 1]
    let lastGuess_ = entityVal <$> eLastGuess
    return $ GameState game_ lastGuess_
    
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
gameStateMessage (GameState (Game secret) (Just (Guess _ word num))) =
    case compare secret word of
        LT -> [st|My word comes before "#{word}" in the dictionary.|]
        GT -> [st|My word comes after "#{word}" in the dictionary.|]
        EQ -> [st|You got it! The word was "#{secret}.
                  You used #{show num} guesses."|]
gameStateMessage (GameState _ Nothing) = "Make a guess!"

guessForm :: Form Text
guessForm = renderDivs $ areq textField "Guess" Nothing

postGameR :: Handler Html
postGameR = error "Not yet implemented: postGameR"
