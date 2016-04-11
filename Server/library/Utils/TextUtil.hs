-- | Offers methods to deal with Strings.
module Utils.TextUtil
( deleteFromText
, shuffleString
, wordToLower
) where

import           Utils.MathUtil
import           Data.List (delete, permutations)
import qualified Data.Text as T
import           Data.Random.RVar (runRVar)
import           Data.Random (StdRandom(..), shuffle)

-- | Deletes all letters from the challenge word in the solution word.
deleteFromText :: String -- ^ challenge word
               -> String -- ^ solution word
               -> String -- ^ hopefully empty word
deleteFromText _ [] = []
deleteFromText [] text = text
deleteFromText (x:xs) text = deleteFromText xs $ delete x text

-- | Shuffles a string randomly.
shuffleString :: String    -- ^ input string
              -> IO String -- ^ shuffled string
shuffleString xs = do
    runRVar (shuffle xs) StdRandom

-- | Converts a String to lowercase.
wordToLower :: String -- ^ word which should be converted to lowercase
            -> String -- ^ word in lowercase
wordToLower = T.unpack . T.toLower . T.pack
