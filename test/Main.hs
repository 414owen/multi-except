module Main where

import Test.Hspec

import qualified Test.MultiExcept.Applicative
import qualified Test.MultiExcept.Alt
import qualified Test.MultiExcept.Functor
import qualified Test.MultiExcept.Bifunctor
import qualified Test.MultiExcept.Foldable
import qualified Test.MultiExcept.Traversable

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  Test.MultiExcept.Functor.spec
  Test.MultiExcept.Applicative.spec
  Test.MultiExcept.Alt.spec
  Test.MultiExcept.Bifunctor.spec
  Test.MultiExcept.Foldable.spec
  Test.MultiExcept.Traversable.spec
