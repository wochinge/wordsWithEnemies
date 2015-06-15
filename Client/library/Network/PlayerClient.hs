{-# LANGUAGE OverloadedStrings #-}

module Network.PlayerClient where

import Network.EasyNetwork
import Types.Player

createPlayer :: String -> IO (Maybe Player)
createPlayer name = post' (server ++ "player") player
	where player = Player Nothing name

insertPlayerInWaitingQueue :: Player -> IO ()
insertPlayerInWaitingQueue p@(Player (Just id) name) = do
    _ <- post' (server ++ "player/" ++ idAsString ++ "/newGame") p
    return ()
    where idAsString = show id