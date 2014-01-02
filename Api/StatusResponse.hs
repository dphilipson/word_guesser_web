module Api.StatusResponse where

import Import
import Game.GameState
import Text.Shakespeare.Text

data StatusResponse = StatusResponse { postMessage :: Text
                                     , postState :: Text
                                     }

instance ToJSON StatusResponse where
    toJSON (StatusResponse message status) =
        object ["message" .= message, "status" .= status]

fromGameState :: GameState -> StatusResponse
fromGameState state = StatusResponse (gameMessage state) (statusEnum state)

statusEnum :: GameState -> Text
statusEnum state
    | stateIsWon state = "WON"
    | stateIsLost state = "LOST"
    | otherwise = "IN_PROGRESS"

gameMessage :: GameState -> Text
gameMessage state
    | stateIsLost state  =
        [st|The secret was #{secret}. Better luck next time!|]
    | otherwise = case stateLatestWord state of
        Just w -> case compare secret w of
            LT -> [st|My word comes before "#{w}" in the dictionary.|]
            GT -> [st|My word comes after "#{w}" in the dictionary.|]
            EQ -> [st|You got it! The word was "#{secret}".
                      You used #{show $ stateGuessCount state} guesses.|]
        Nothing -> "I'm thinking of a word. Make a guess!"
  where secret = stateSecret state


