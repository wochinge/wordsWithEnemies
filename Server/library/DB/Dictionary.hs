{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DB.Dictionary (createTables, wordExists, getRandomWord) where

import           Control.Applicative
import           Control.Monad
import qualified Database.SQLite.Simple as S
import           Snap.Snaplet
import           Snap.Snaplet.SqliteSimple
import qualified Data.Text as T
import           Application
import           DB.Utils
import qualified Data.List as L

-- | Creates the dictionary table and fills it with the words from the text file.
createTables :: S.Connection -- ^ database connection
             -> IO ()        -- ^ nothing
createTables conn = do
    S.execute_ conn "BEGIN"
    
    createTable conn "dictionary" $
         T.concat [ "CREATE TABLE dictionary ("
                  , "word_id INTEGER PRIMARY KEY, "
                  , "word TEXT NOT NULL)"
                  ]
    schemaCreated <- tableExists conn "dictionary"
    unless schemaCreated $ do
      words <- readWords
      mapM_ (insertWord conn) $ filteredWords words
    
    S.execute_ conn "COMMIT"

-- | Reads the words from a text file.
readWords :: IO [String] -- ^ list of words (words are lowercase, for a later not case sensitive use)
readWords = do
    words <- readFile "wordlist.txt"
    let wordList = lines words
    return $ map (T.unpack . T.toLower . T.pack) wordList

-- | Filters the words, because word version with "'" are not wanted.
filteredWords :: [String] -- ^ unfiltered word list
              -> [String] -- ^ filtered word list
filteredWords words = filter (\a -> not $ "'" `L.isInfixOf` a) words
    
-- | Inserts a word in the database.
insertWord :: S.Connection -- ^ database connection
           -> String       -- ^ word to insert
           -> IO ()        -- ^ nothing
insertWord conn word = do
    S.execute conn "INSERT INTO dictionary (word) VALUES (?)" (Only (word))

-- | Checks if a given word is valid by looking whether the word is in the given word list.
-- | The check is not case sensitive!
wordExists :: String -- ^ word to check
           -> Handler App Sqlite Bool -- ^ True if word is in dictionary, Else if not
wordExists word = do
    result <- query "SELECT 1 FROM dictionary WHERE word = ? LIMIT 1" (Only (lowerCaseWord)) :: Handler App Sqlite [Only (Int)]
    return $ (length result) > 0
    where
        lowerCaseWord = T.unpack $ T.toLower $ T.pack word

-- | Returns a random word out of the dictionary.
-- | The word must have a length of >=5, so that there are enough letters to build new words.
getRandomWord :: Handler App Sqlite String -- ^ random word
getRandomWord = do
    result <- query_ "SELECT word FROM dictionary WHERE LENGTH(word) >= 5 ORDER BY RANDOM() LIMIT 1"
    return $ fromOnly $ head result
