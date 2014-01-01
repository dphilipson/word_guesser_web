module Handler.Game where

import Import
import Text.Shakespeare.Text

data GameState = GameState { stateGameEntity :: Entity Game
                           , stateLatestGuess :: Maybe Guess
                           }

stateSecret :: GameState -> Text
stateSecret (GameState {stateGameEntity = Entity _ (Game {gameSecret = secret})}) = secret

getGameR :: Handler Html
getGameR = do
    gameState <- runDB $ loadGameState
    let latestMessage = gameStateMessage gameState    
    (formWidget, formEnctype) <- generateFormPost guessForm
    defaultLayout $(widgetFile "game")

loadGameState :: YesodDB App GameState
loadGameState = do
    -- Assume just one game for now.
    gEntity@(Entity gid _) <- loadGameEntity
    eLastGuess <- selectFirst [GuessGame ==. gid] [Desc GuessCount, LimitTo 1]
    let lastGuess = entityVal <$> eLastGuess
    return $ GameState gEntity lastGuess
    
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
gameStateMessage (GameState _ Nothing) = "Make a guess!"
gameStateMessage gameState@(GameState _ (Just (Guess _ word num))) =
    case compare secret word of
        LT -> [st|My word comes before "#{word}" in the dictionary.|]
        GT -> [st|My word comes after "#{word}" in the dictionary.|]
        EQ -> [st|You got it! The word was "#{secret}.
                  You used #{show num} guesses."|]
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
    let c = nextCount gameState
        Entity gameId _ = stateGameEntity gameState
    insert_ $ Guess gameId word c

nextCount :: GameState -> Int
nextCount GameState {stateLatestGuess = Nothing} = 1
nextCount GameState {stateLatestGuess = Just (Guess _ _ c)} = c + 1
