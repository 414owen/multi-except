module Test.MultiExcept.Foldable
  ( spec
  ) where

import Test.Hspec

import qualified Data.Foldable as Foldable
import Control.Applicative.MultiExcept

spec :: Spec
spec = describe "Foldable instance" $ do
  it "f is called for success" $
    Foldable.foldr (+) 2 (succeed 1) `shouldBe` (3 :: Int)
  it "acc is returned for errors" $
    Foldable.foldr (+) 2 (throwError (1 :: Int)) `shouldBe` (2 :: Int)
