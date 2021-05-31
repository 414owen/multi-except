{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Test.Functor
  ( spec
  ) where

import Test.Hspec

import Control.Applicative.MultiExcept

testErrors :: MultiExcept Int Int
testErrors = fromEitherPoly (Left [2, 3])

spec :: Spec
spec = describe "Functor instance" $ do
  it "transforms success value" $
    ((+ 3) <$> succeed 2) `shouldBe` succeed @() 5
  it "doesn't affect errors" $ do
    fmap (+ 3) testErrors `shouldBe` testErrors
