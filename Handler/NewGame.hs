module Handler.NewGame where

import Import
import Api.StatusResponse
import Game.GameState
import Yesod.Auth

postNewGameR :: Handler Value
postNewGameR = do
    mUserId <- maybeAuthId
    case mUserId of
        Just userId -> do
            runDB $ do
                deleteGame userId
                state <- loadGameState userId
                return $ toJSON $ fromGameState $ state
        Nothing -> return Null

deleteGame :: UserId -> YesodDB App ()
deleteGame userId = do
    mEntity <- getBy $ UniqueGame userId
    case mEntity of
        Just (Entity gameId _) -> deleteCascade gameId
        Nothing -> return ()
