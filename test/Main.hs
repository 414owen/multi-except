module Main where

import Test.Hspec

import qualified Test.Applicative
import qualified Test.Alt
import qualified Test.Functor

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  Test.Functor.spec
  Test.Applicative.spec
  Test.Alt.spec
