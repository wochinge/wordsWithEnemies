{-# LANGUAGE OverloadedStrings #-}

module Network.GameClient where

import Network.EasyNetwork
import Types.Player
import Types.Game
import Data.Maybe
import Types.Round

getStatus :: Player -> IO (Maybe Game)
getStatus player = get' (server ++ "player/" ++ id ++ "/status") 
	where 
        id = show $ fromJust $ playerId player
          
postSolution :: String -> Game -> IO ()
postSolution solution game = do
    post' (server ++ "game/" ++ gId ++ "round/" ++ rId ++ "/solution") solution
    return ()
    where 
        rId = show $ roundId $ last $ rounds game
        gId = show $ gameId game