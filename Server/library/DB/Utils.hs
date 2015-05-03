{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module DB.Utils where

import           Control.Applicative
import           Control.Monad
import qualified Database.SQLite.Simple as S
import           Snap.Snaplet
import           Snap.Snaplet.SqliteSimple

tableExists :: S.Connection -> String -> IO Bool
tableExists con tableName = do
    r <- S.query con "SELECT name FROM sqlite_master WHERE type='table' AND name=?" (Only tableName)
    case r of
         [Only (_ :: String)] -> return True
         _ -> return False
