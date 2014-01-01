module Game.Lexicon
    (
      Lexicon(..)
    , randomSecret
    , isValidGuess
    , loadLexicon
    ) where

import Import
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import System.Random

data Lexicon = Lexicon { allowedSecrets :: Vector Text
                       , allowedGuesses :: Set Text }

randomSecret :: Lexicon -> IO Text
randomSecret = selectRandom . allowedSecrets

isValidGuess :: Text -> Lexicon -> Bool
isValidGuess guess = S.member (canonicalize guess) . allowedGuesses

loadLexicon :: FilePath -> FilePath -> IO Lexicon
loadLexicon secretFile guessFile =
    Lexicon <$> linesVector secretFile <*> linesSet guessFile

linesVector :: FilePath -> IO (Vector Text)
linesVector = (V.fromList <$>) . canonicalLines

linesSet :: FilePath -> IO (Set Text)
linesSet = (S.fromList <$>) . canonicalLines

canonicalLines :: FilePath -> IO [Text]
canonicalLines = ((map canonicalize . T.lines) <$>) . TIO.readFile

canonicalize :: Text -> Text
canonicalize = T.toLower . T.strip

selectRandom :: Vector Text -> IO Text
selectRandom v = ((v !) . fst . randomR (0, V.length v - 1)) <$> newStdGen
