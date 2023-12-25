{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Test.MultiExcept.Foldable
  ( spec
  ) where

import Test.Hspec

import Control.Applicative.MultiExcept

spec :: Spec
spec = describe "Foldable instance" $ do
  it "f is called for success" $
    foldr (+) 2 (succeed 1)`shouldBe` 3
  it "acc is returned for errors" $
    foldr (+) 2 (throwError 1)`shouldBe` 2
