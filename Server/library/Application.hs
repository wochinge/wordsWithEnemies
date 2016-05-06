{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Module, which defines and holds several sub snaplets.
module Application where

import           Control.Lens
import           Snap
import           Snap.Snaplet
import           Api.PlayerApp
import           Api.GameApp
import           Snap.Snaplet.SqliteSimple
import           Control.Monad.State

-- | Holds the snaplets.
data App = App { _playerSnaplet :: Snaplet PlayerApp -- ^ player api
               , _gameSnaplet :: Snaplet GameApp     -- ^ game api
               , _playerDAO :: Snaplet Sqlite
               , _dictionary :: Snaplet Sqlite
               , _gameDAO :: Snaplet Sqlite
               , _roundDAO :: Snaplet Sqlite
               , _scoreDAO :: Snaplet Sqlite
               , _solutionDAO :: Snaplet Sqlite
               }

-- | Creates the snaplet.
makeLenses ''App

-- | Provides a HasSqlite instance for this module.
instance HasSqlite (Handler b App) where
    getSqliteState = with playerDAO get
