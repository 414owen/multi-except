{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE OverloadedLists  #-}

module Test.MultiExcept.Alt
  ( spec
  ) where

import Test.Hspec

import Data.Functor.Alt
import Control.Applicative.MultiExcept
import Control.Applicative.MultiExcept.Alt ()

spec :: Spec
spec = describe "Alt instance" $ do
  it "propagates left succeed" $ do
    succeed 1 <!> throwError () `shouldBe` succeed (1 :: Int)
  it "propagates right succeed" $
    throwError () <!> succeed 1 `shouldBe` succeed (1 :: Int)
  it "prioritizes left succeed" $
    succeed 1 <!> succeed 2 `shouldBe` (succeed 1 :: MultiExcept () Int)
  it "propagates both errors" $ do
    throwError 1 <!> throwError 2 `shouldBe` (throwErrors [1, 2] :: MultiExcept Int ())
