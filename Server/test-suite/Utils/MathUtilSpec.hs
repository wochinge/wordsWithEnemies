{-# LANGUAGE ScopedTypeVariables #-}
module Utils.MathUtilSpec (spec) where

import           Utils.MathUtil
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = describe "tests method fac" $ do
        it "calculates the faculty of 5" $
            fac 5 `shouldBe` 120
        it "calculates the faculty of 0" $
            fac 0 `shouldBe` 1
        it "calculates the faculty of 1" $
            fac 1 `shouldBe` 1
        it "calculates the faculty of 2" $
            fac 2 `shouldBe` 2