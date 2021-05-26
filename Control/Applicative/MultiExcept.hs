{-|
Module      : Control.Applicative.MultiExcept
Copyright   : (c) Owen Shepherd, 2021
License     : MIT
Maintainer  : owen@owen.cafe
Stability   : stable
Portability : portable
-}

module Control.Applicative.MultiExcept
  ( MultiExcept
  , fromEither
  , runMultiExcept
  , succeed
  , throwError
  ) where

import Data.Functor.Alt
import Data.DList.DNonEmpty (DNonEmpty)
import qualified Data.DList.DNonEmpty as DNE
import Data.List.NonEmpty (NonEmpty)

-- | A MultiExcept is a success value, or one or more errors
data MultiExcept err a
  = Success a
  | Errors (DNonEmpty err)
  deriving (Eq, Ord, Read, Show)

-- | Run the computation
runMultiExcept :: MultiExcept err a -> Either (NonEmpty err) a
runMultiExcept (Errors errs) = Left $ DNE.toNonEmpty errs
runMultiExcept (Success a) = Right a

-- | Throw a single error
throwError :: err -> MultiExcept err a
throwError = Errors . pure

-- | Embeds a value into a MultiExcept context
succeed :: a -> MultiExcept err a
succeed a = Success a

-- | Convert an Either to a MultiExcept
fromEither :: Either err a -> MultiExcept err a
fromEither (Left err) = Errors (DNE.singleton err)
fromEither (Right a) = Success a

instance Functor (MultiExcept err) where
  fmap f (Success a) = Success $ f a
  fmap _ (Errors errs) = Errors errs

instance Applicative (MultiExcept err) where
  pure = succeed

  Errors l <*> Errors l' = Errors $ l <> l'
  Success f <*> Success a = Success $ f a
  Errors l <*> _ = Errors l
  _ <*> Errors l = Errors l

instance Alt (MultiExcept err) where
  Success a <!> _ = Success a
  _ <!> Success a = Success a
  Errors l <!> Errors r = Errors (l <> r)
