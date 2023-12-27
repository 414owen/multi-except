module Test.MultiExcept.Applicative
  ( spec
  ) where

import Test.Hspec

import Prelude (Int, ($), Num(..))

import Control.Applicative (Applicative(..))
import Control.Applicative.MultiExcept

testErrors :: MultiExcept Int Int
testErrors = throwError 2 *> throwError 3

spec :: Spec
spec = describe "Applicative instance" $ do
  describe "pure" $
    it "succeeds" $
      pure 5 `shouldBe` (succeed 5 :: MultiExcept () Int)
  describe "<*>" $ do
    it "accumulates errors" $
      throwError 2 <*> throwError 3 `shouldBe` testErrors
    it "propagates successes" $
      pure (3,4) `shouldBe` (succeed (3, 4) :: MultiExcept () (Int, Int))
    it "errors when only one side is successful" $ do
      succeed (+ 1) <*> testErrors `shouldBe` testErrors
      throwError 2 <*> succeed () `shouldBe` (throwError 2 :: MultiExcept Int Int)
