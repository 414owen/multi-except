{-# LANGUAGE CPP             #-}
{-# LANGUAGE OverloadedLists #-}

module Test.MultiExcept.Bifunctor
  ( spec
  ) where

import Test.Hspec

#if MIN_VERSION_base(4,8,0)

import Data.Bifunctor
import Control.Applicative.MultiExcept

spec :: Spec
spec = describe "Bifunctor instance" $ do
  it "maps errors with first" $
    first (+ 1) (throwErrors [3, 4]) `shouldBe` (throwErrors [4, 5] :: MultiExcept Int ())
  it "maps successes with second" $
    second (+ 1) (succeed 3) `shouldBe` (succeed 4 :: MultiExcept Int Int)

#else

spec :: Spec
spec = pure ()

#endif
