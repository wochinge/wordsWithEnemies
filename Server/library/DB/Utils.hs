{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module with functions for easy database handling.
module DB.Utils 
( tableExists
, createTable
, DatabaseId
) where

import           Control.Monad
import qualified Database.SQLite.Simple as S
import           Snap.Snaplet.SqliteSimple
import           Data.Text

-- | Name of a table.
type Table           = String
-- | SQL-Statement.
type CreateStatement = Text
-- | Type for ids in databases.
type DatabaseId      = Integer

-- | Checks whether a tables already exists in the database.
tableExists :: S.Connection -- ^ database connection
            -> Table        -- ^ name of the table
            -> IO Bool      -- ^ true if it exists already, otherwise false
tableExists con tableName = do
    r <- S.query con "SELECT name FROM sqlite_master WHERE type='table' AND name=?" (Only tableName)
    case r of
        [Only (_ :: String)] -> return True
        _ -> return False

-- | Creates a table in the database.
createTable :: S.Connection    -- ^ database connection
            -> Table           -- ^ name of the table
            -> CreateStatement -- ^ sql statement to create the table
            -> IO ()           -- ^ nothing
createTable conn name statement = do
    tableCreated <- tableExists conn name
    unless tableCreated $
      S.execute_ conn $ S.Query statement