{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE TypeApplications #-}

module Test.MultiExcept.Alt
  ( spec
  ) where

import Test.Hspec

import Data.Functor.Alt
import Control.Applicative.MultiExcept
import Control.Applicative.MultiExcept.Alt ()

one :: Int
one = 1

two :: Int
two = 2

spec :: Spec
spec = describe "Alt instance" $ do
  it "propagates left succeed" $ do
    succeed one <!> throwError two `shouldBe` succeed 1
  it "propagates right succeed" $
    throwError () <!> succeed 1 `shouldBe` succeed one
  it "prioritizes left succeed" $
    succeed 1 <!> succeed 2 `shouldBe` succeed @() one
  it "propagates both errors" $ do
    throwError 1 <!> throwError 2 `shouldBe` throwErrors @() [one, 2]
