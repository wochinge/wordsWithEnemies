{-# LANGUAGE OverloadedStrings #-}

module Network.PlayerClient where

import Network.EasyNetwork
import Types.Player

createPlayer :: String -> IO (Maybe Player)
createPlayer name = post' "http://localhost:9000/user" player
	where player = Player Nothing name
