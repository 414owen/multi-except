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
  , runMultiExcept
  , throwError
  , succeed
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.DList.DNonEmpty (DNonEmpty)
import qualified Data.DList.DNonEmpty as DNE

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
throwError = Errors . DNE.singleton

-- | Embeds a value into a MultiExcept context
succeed :: a -> MultiExcept err a
succeed a = Success a

instance Functor (MultiExcept err) where
  fmap f (Success a) = Success $ f a
  fmap _ (Errors errs) = Errors errs

instance Applicative (MultiExcept err) where
  pure = succeed

  Errors l <*> Errors l' = Errors $ DNE.append l l'
  Success f <*> Success a = Success $ f a
  Errors l <*> _ = Errors l
  _ <*> Errors l = Errors l
