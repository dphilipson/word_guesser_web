module Handler.Surrender where

import Import
import Yesod.Auth

postSurrenderR :: Handler Html
postSurrenderR = do
    mUserId <- maybeAuthId
    case mUserId of
        Just userId -> do
            runDB $ surrender userId
            redirect GameR
        Nothing -> redirect $ AuthR LoginR

surrender :: UserId -> YesodDB App ()
surrender userId = updateWhere [GameOwner ==. userId] [GameSurrendered =. True]
