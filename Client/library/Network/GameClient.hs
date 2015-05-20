{-# LANGUAGE OverloadedStrings #-}

module Network.GameClient where

import Network.EasyNetwork
import Types.Player
import Types.Game
import Data.Maybe

getStatus :: Player -> IO (Maybe Game)
getStatus player = get' (server ++ "player/" ++ id ++ "/status") 
	where 
          id = show $ fromJust $ playerId player