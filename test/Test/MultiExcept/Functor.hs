module Test.MultiExcept.Functor
  ( spec
  ) where

import Test.Hspec

import Prelude hiding (Applicative(..))
import Control.Applicative (Applicative(..))
import Control.Applicative.MultiExcept

testErrors :: MultiExcept Int Int
testErrors = throwError 2 *> throwError 3

spec :: Spec
spec = describe "Functor instance" $ do
  it "transforms success value" $
    ((+ 3) <$> succeed 2) `shouldBe` (succeed 5 :: MultiExcept () Int)
  it "doesn't affect errors" $ do
    fmap (+ 3) testErrors `shouldBe` testErrors
