{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DB.Utils where

import           Control.Applicative
import           Control.Monad
import qualified Database.SQLite.Simple as S
import           Snap.Snaplet
import           Snap.Snaplet.SqliteSimple
import           Data.Text

type Table           = String
type CreateStatement = Text

tableExists :: S.Connection -> Table -> IO Bool
tableExists con tableName = do
    r <- S.query con "SELECT name FROM sqlite_master WHERE type='table' AND name=?" (Only tableName)
    case r of
        [Only (_ :: String)] -> return True
        _ -> return False

createTable :: S.Connection -> Table -> CreateStatement -> IO ()
createTable conn name statement = do
    tableCreated <- tableExists conn name
    unless tableCreated $
      S.execute_ conn $ S.Query statement