module Main where

import Test.Hspec

import qualified Test.Applicative
import qualified Test.Alt
import qualified Test.Functor
import qualified Test.Bifunctor
import qualified Test.Foldable
import qualified Test.Traversable

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  Test.Functor.spec
  Test.Applicative.spec
  Test.Alt.spec
  Test.Bifunctor.spec
  Test.Foldable.spec
  Test.Traversable.spec
