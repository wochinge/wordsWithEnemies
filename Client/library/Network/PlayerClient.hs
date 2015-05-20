{-# LANGUAGE OverloadedStrings #-}

module Network.PlayerClient where

import Network.EasyNetwork
import Types.Player

createPlayer :: String -> IO (Maybe Player)
createPlayer name = post' (server ++ "player") player
	where player = Player Nothing name
