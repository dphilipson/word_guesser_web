module Handler.Surrender where

import Import
import Api.StatusResponse
import Game.GameState
import Yesod.Auth

postSurrenderR :: Handler Value
postSurrenderR = do
    mUserId <- maybeAuthId
    case mUserId of
        Just userId -> do
            runDB $ do
                surrender userId
                state <- loadGameState userId
                return $ toJSON $ fromGameState $ state
        Nothing -> return Null

surrender :: UserId -> YesodDB App ()
surrender userId = updateWhere [GameOwner ==. userId] [GameSurrendered =. True]
