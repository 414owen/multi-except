module Test.MultiExcept.Traversable
  ( spec
  ) where

import Prelude   (Int, Maybe(..), ($), const)

import Data.Traversable
import Test.Hspec
import Control.Applicative.MultiExcept

spec :: Spec
spec = describe "Traversable instance" $ do
  it "Just works™" $
    traverse Just (succeed 3) `shouldBe` Just (succeed 3 :: MultiExcept () Int)
  it "leaves errors" $
    traverse Just (throwError 3) `shouldBe` Just (throwError 3 :: MultiExcept Int ())
  it "Nothing works™" $
    traverse (const Nothing :: a -> Maybe a) (succeed 3 :: MultiExcept () Int) `shouldBe` Nothing
