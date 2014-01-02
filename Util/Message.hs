module Util.Message
    ( 
      setMessageSuccess
    , setMessageInfo
    , setMessageWarning
    , setMessageDanger
    ) where

import Prelude
import Yesod
import Data.Text

data MessageLevel = Success | Info | Warning | Danger

setMessageLevel :: MonadHandler m => MessageLevel -> Html -> m ()
setMessageLevel level message =
    setMessage [shamlet|<div class="alert #{levelClass}">#{message}|]
  where levelClass :: Text
        levelClass = case level of
            Success -> "alert-success"
            Info -> "alert-info"
            Warning -> "alert-warning"
            Danger -> "alert-danger"

setMessageSuccess :: MonadHandler m => Html -> m ()
setMessageSuccess = setMessageLevel Success

setMessageInfo :: MonadHandler m => Html -> m ()
setMessageInfo = setMessageLevel Info

setMessageWarning :: MonadHandler m => Html -> m ()
setMessageWarning = setMessageLevel Warning

setMessageDanger :: MonadHandler m => Html -> m ()
setMessageDanger = setMessageLevel Danger
