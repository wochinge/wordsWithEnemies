{-# LANGUAGE ScopedTypeVariables #-}
module GameSpec (spec) where

import           Game
import qualified Types.Game as G
import qualified Types.Player as P
import qualified Types.Round as R
import qualified Types.Score as Score
import qualified Types.Solution as Solution
import           Test.Hspec
import           Test.QuickCheck
import           Data.Maybe


player1 = P.Player (Just 1) "TestPlayer1"
player2 = player1 {P.playerId = Just 2}
players = [player1, player2]

score1 = Score.Score (Just 1) 5 player1

solutions1 = [Solution.Solution (Just 1) "lala" player1, Solution.Solution (Just 2) "baba" player2]

rounds1 = [R.Round (Just 1) (Just 1) "Test" (Just score1) solutions1, R.Round (Just 2) (Just 2) "Test" (Just score1) solutions1]
rounds2 = [R.Round (Just 1) (Just 1) "Test" (Just score1) solutions1]
rounds3 = [R.Round (Just (-1)) (Just (-1)) "Test" (Just score1) solutions1, R.Round (Just 1) (Just 1) "Test" (Just score1) solutions1]

game1 = G.Game (Just 1) players False rounds1
game2 = G.Game (Just 2) players False rounds2
game3 = G.Game (Just 3) players False []
game4 = G.Game (Just 4) players False rounds3


spec :: Spec
spec = do
    describe "tests method maxRoundNr" $ do
        it "get maxRoundNr out of 1 and 2" $
            maxRoundNr game1 `shouldBe` 2
        it "maxRoundNr of 1" $
            maxRoundNr game2 `shouldBe` 1
        it "maxRoundNr of negative number" $
            maxRoundNr game4 `shouldBe` 1
    
    describe "tests method totalScores" $ do
        it "totalScores of 2 players" $
            totalScores player1 game1 `shouldBe` (10,0)
        it "totalScores of 2 players" $
            totalScores player2 game1 `shouldBe` (0,10)
        it "totalScores empty rounds" $
            totalScores player1 game3 `shouldBe` (0,0)