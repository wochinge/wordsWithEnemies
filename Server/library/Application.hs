{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

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
               }

makeLenses ''App

instance HasSqlite (Handler b App) where
    getSqliteState = with playerDb get
