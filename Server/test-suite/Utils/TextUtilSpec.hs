{-# LANGUAGE ScopedTypeVariables #-}
module Utils.TextUtilSpec (spec) where

import           Utils.TextUtil
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = describe "tests method wordToLower" $ do
        it "wordToLower of HOUSE" $
            wordToLower "HOUSE" `shouldBe` "house"
        it "wordToLower of duCk" $
            wordToLower "duCk" `shouldBe` "duck"
        it "wordToLower of lower" $
            wordToLower "lower" `shouldBe` "lower"
        it "wordToLower of I" $
            wordToLower "I" `shouldBe` "i"