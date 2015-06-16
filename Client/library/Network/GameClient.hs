{-# LANGUAGE OverloadedStrings #-}

module Network.GameClient where

import Network.EasyNetwork
import Types.Player
import Types.Game
import Data.Maybe
import Types.Round
import Types.Solution

-- | If there a two players gets the game else nothing.
getStatus :: Player          -- ^ player searching for a teammate
          -> IO (Maybe Game) -- ^ game if there is one
getStatus player = get' (server ++ "player/" ++ id ++ "/status") 
	where 
        id = show $ fromJust $ playerId player

-- | Sends the solution of the player to the server.
postSolution :: Solution -- ^ solution of the player
             -> Game     -- ^ current game
             -> IO ()    -- ^ nothing
postSolution solution game = do
    _ <- post' (server ++ "game/" ++ gId ++ "/round/" ++ rId ++ "/solution") solution
    return ()
    where 
        rId = show $ fromJust $ roundId $ last $ rounds game
        gId = show $ fromJust $ gameId game

-- | Gets the game with a new round, including the outcome of the last round.
-- | If both players already pushed their solutions.
getGameWithNewRound :: Round -- ^ last round
                    -> Game  -- ^ current game
                    -> IO (Maybe Game) -- ^ game with new round 
getGameWithNewRound lastRound game = get' (server ++ "game/" ++ gId ++ "/round/newRound/" ++ rNr) 
	where 
        rNr = show $ fromJust $ roundNr lastRound
        gId = show $ fromJust $ gameId game