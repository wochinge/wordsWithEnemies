{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Application where

import Control.Lens
import Snap.Snaplet
import Snap
import Api.PlayerApp
import Snap.Snaplet.SqliteSimple


data App = App {_userSnaplet :: Snaplet UserApp,
                _db :: Snaplet Sqlite
               }

makeLenses ''App

instance HasSqlite (Handler b App) where
    getSqliteState = with db get
