module Handler.NewGame where

import Import
import Yesod.Auth

postNewGameR :: Handler Html
postNewGameR = do
    mUserId <- maybeAuthId
    case mUserId of
        Just userId -> do
            runDB $ deleteGame userId
            redirect GameR
        Nothing -> redirect $ AuthR LoginR

deleteGame :: UserId -> YesodDB App ()
deleteGame userId = do
    mEntity <- getBy $ UniqueGame userId
    case mEntity of
        Just (Entity gameId _) -> deleteCascade gameId
        Nothing -> return ()
