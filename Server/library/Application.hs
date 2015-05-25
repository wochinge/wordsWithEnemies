{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Module, which defines and holds several sub snaplets.
module Application where

import           Control.Lens
import           Snap.Snaplet
import           Snap
import           Api.PlayerApp
import			 Api.GameApp
import           Snap.Snaplet.SqliteSimple

data App = App { _playerSnaplet :: Snaplet PlayerApp
			   , _gameSnaplet :: Snaplet GameApp
               , _playerDAO :: Snaplet Sqlite
               , _dictionary :: Snaplet Sqlite
               , _gameDAO :: Snaplet Sqlite
               , _roundDAO :: Snaplet Sqlite
               , _scoreDAO :: Snaplet Sqlite
               , _solutionDAO :: Snaplet Sqlite
               }

makeLenses ''App

-- | Provides a HasSqlite instance for this module.
instance HasSqlite (Handler b App) where
    getSqliteState = with playerDAO get
