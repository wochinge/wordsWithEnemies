{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

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
    
createTables :: S.Connection -> IO ()
createTables conn = do
  schemaCreated <- tableExists conn "dictionary"
  unless schemaCreated $
    S.execute_ conn
      (S.Query $
       T.concat [ "CREATE TABLE dictionary ("
                , "word_id INTEGER PRIMARY KEY, "
                , "word TEXT NOT NULL)"])
  unless schemaCreated $ do
    words <- readWords
    mapM_ (insertWord conn) $ filteredWords words

readWords :: IO [String]
readWords = do
    words <- readFile "wordlist.txt"
    return $ lines words

filteredWords :: [String] -> [String]
filteredWords words = filter (\a -> not $ "'" `L.isInfixOf` a) words
    
insertWord :: S.Connection -> String -> IO ()
insertWord conn word = do
    S.execute conn "INSERT INTO dictionary (word) VALUES (?)" (Only (word))
    
wordExists :: String -> Handler App Sqlite Bool
wordExists word = do
    result <- query "SELECT 1 FROM dictionary WHERE word = ?" (Only (word))
    case result of
         [Only (_ :: String)] -> return True
         _ -> return False

getRandomWord :: Handler App Sqlite String
getRandomWord = do
    result <- query_ "SELECT word FROM dictionary WHERE LENGTH(word) >= 5 ORDER BY RANDOM() LIMIT 1"
    case result of
         [Only (a :: String)] -> return a
         _ -> return ""
