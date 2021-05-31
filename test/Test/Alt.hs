{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Test.Alt
  ( spec
  ) where

import Test.Hspec

import Data.Functor.Alt
import Control.Applicative.MultiExcept

spec :: Spec
spec = describe "Alt instance" $ do
  it "propagates left succeed" $
    succeed 1 <!> throwError 3 `shouldBe` succeed 1
  it "propagates right succeed" $
    throwError () <!> succeed 1 `shouldBe` succeed 1
  it "prioritizes left succeed" $
    succeed 1 <!> succeed 2 `shouldBe` succeed @() 1
  it "propagates both errors" $ do
    throwError 1 <!> throwError 2`shouldBe` throwErrors @() [1, 2]
