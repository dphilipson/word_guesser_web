module Handler.Home where

import Import
import Data.Maybe
import Yesod.Auth

getHomeR :: Handler Html
getHomeR = do
    loggedIn <- isJust <$> maybeAuthId
    defaultLayout $ do
        setTitle "Home - Word Guesser"
        $(widgetFile "home")
