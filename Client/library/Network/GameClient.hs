{-# LANGUAGE OverloadedStrings #-}

module Network.GameClient where

import Network.EasyNetwork
import Types.Player
import Types.Game
import Data.Maybe
import Types.Round
import Types.Solution

getStatus :: Player -> IO (Maybe Game)
getStatus player = get' (server ++ "player/" ++ id ++ "/status") 
	where 
        id = show $ fromJust $ playerId player
          
postSolution :: Solution -> Game -> IO ()
postSolution solution game = do
    post' (server ++ "game/" ++ gId ++ "/round/" ++ rId ++ "/solution") solution
    return ()
    where 
        rId = show $ fromJust $ roundId $ last $ rounds game
        gId = show $ fromJust $ gameId game

getGameWithNewRound :: Round -> Game -> IO (Maybe Game)
getGameWithNewRound lastRound game = get' (?) 
	where 
        rId = show $ fromJust $ roundId lastRound
        gId = show $ fromJust $ gameId game