{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Module, which defines and holds several sub snaplets.
module Application where

import           Control.Lens
import           Snap.Snaplet
import           Snap
import           Api.PlayerApp
import           Snap.Snaplet.SqliteSimple

data App = App { _playerSnaplet :: Snaplet PlayerApp
               , _playerDb :: Snaplet Sqlite
               , _dictionary :: Snaplet Sqlite
               , _gameDb :: Snaplet Sqlite
               , _roundDb :: Snaplet Sqlite
               , _scoreDb :: Snaplet Sqlite
               , _solutionDb :: Snaplet Sqlite
               }

makeLenses ''App

-- | Provides a HasSqlite instance for this module.
instance HasSqlite (Handler b App) where
    getSqliteState = with playerDb get
