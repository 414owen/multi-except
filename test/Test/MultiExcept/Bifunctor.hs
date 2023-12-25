{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Test.MultiExcept.Bifunctor
  ( spec
  ) where

import Test.Hspec

import Data.Bifunctor
import Control.Applicative.MultiExcept

spec :: Spec
spec = describe "Bifunctor instance" $ do
  it "maps errors with first" $
    first (+ 1) (throwErrors [3, 4]) `shouldBe` throwErrors @() [4, 5]
  it "maps successes with second" $
    second (+ 1) (succeed 3) `shouldBe` succeed @() 4
