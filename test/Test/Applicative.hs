{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Test.Applicative
  ( spec
  ) where

import Test.Hspec

import Control.Applicative.MultiExcept

testErrors :: MultiExcept Int Int
testErrors = fromEitherPoly (Left [2, 3])

spec :: Spec
spec = describe "Applicative instance" $ do
  describe "pure" $
    it "succeeds" $
      pure 5 `shouldBe` (succeed 5 :: MultiExcept () Int)
  describe "<*>" $ do
    it "accumulates errors" $
      throwError 2 <*> throwError 3 `shouldBe` testErrors
    it "propagates successes" $
      (,) <$> pure 3 <*> pure 4 `shouldBe` succeed @() (3, 4)
