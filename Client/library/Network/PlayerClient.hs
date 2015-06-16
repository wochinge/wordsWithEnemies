{-# LANGUAGE OverloadedStrings #-}

-- | A module which sends player related things to the server.
module Network.PlayerClient where

import Network.EasyNetwork
import Types.Player

-- | Sends name of a new player to the server.
-- | Gets back a player object.
createPlayer :: String            -- ^ nickname of the player
             -> IO (Maybe Player) -- ^ Player object if created
createPlayer name = post' (server ++ "player") player
	where player = Player Nothing name

-- | Sends the player to the servers waiting list.
insertPlayerInWaitingQueue :: Player -> IO ()
insertPlayerInWaitingQueue p@(Player (Just id) name) = do
    _ <- post' (server ++ "player/" ++ idAsString ++ "/newGame") p
    return ()
    where idAsString = show id