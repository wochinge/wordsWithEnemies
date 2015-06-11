{-# LANGUAGE ScopedTypeVariables #-}
module Utils.TextUtilSpec (spec) where

import           Utils.TextUtil
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
    describe "tests method wordToLower" $ do
        it "wordToLower of HOUSE" $
            wordToLower "HOUSE" `shouldBe` "house"
        it "wordToLower of duCk" $
            wordToLower "duCk" `shouldBe` "duck"
        it "wordToLower of lower" $
            wordToLower "lower" `shouldBe` "lower"
        it "wordToLower of I" $
            wordToLower "I" `shouldBe` "i"
    
    describe "tests method deleteFromText" $ do
        it "a deleteFromText ab" $
            deleteFromText "a" "ab" `shouldBe` "b"
        it "house deleteFromText mouse" $
            deleteFromText "house" "mouse" `shouldBe` "m"
        it "house deleteFromText Tilda" $
            deleteFromText "house" "Tilda" `shouldBe` "Tilda"
        it "h deleteFromText H" $
            deleteFromText "h" "H" `shouldBe` "H"
        