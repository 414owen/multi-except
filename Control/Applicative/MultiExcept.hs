{-|
Module      : Control.Applicative.MultiExcept
Copyright   : (c) Owen Shepherd, 2021
License     : MIT
Maintainer  : owen@owen.cafe
Stability   : stable
Portability : portable
-}

{-# LANGUAGE ScopedTypeVariables #-}

module Control.Applicative.MultiExcept
  ( MultiExcept
  , fromEither
  , fromEitherPoly
  , join
  , runMultiExcept
  , succeed
  , throwError
  , throwErrors
  ) where

import Data.Bifunctor
import Data.Functor.Alt
import Data.DList.DNonEmpty (DNonEmpty)

-- | A 'MultiExcept' is a success value, or one or more errors.
data MultiExcept err a
  = Success a
  | Errors (DNonEmpty err)
  deriving (Eq, Ord, Read, Show)

-- | Run the computation.
runMultiExcept :: MultiExcept err a -> Either (DNonEmpty err) a
runMultiExcept (Errors errs) = Left errs
runMultiExcept (Success a) = Right a

-- | Throw a single error.
throwError :: forall a err. err -> MultiExcept err a
throwError = Errors . pure

-- | Throw one or more errors.
throwErrors :: forall a err. DNonEmpty err -> MultiExcept err a
throwErrors = Errors

-- | Embeds a value into a 'MultiExcept' context.
succeed :: forall err a. a -> MultiExcept err a
succeed = Success

-- | Convert an 'Either' to a 'MultiExcept'.
fromEither :: Either err a -> MultiExcept err a
fromEither (Left err) = throwError err
fromEither (Right a) = Success a

-- | Convert a multi-error 'Either' to a 'MultiExcept'.
fromEitherPoly :: Either (DNonEmpty err) a -> MultiExcept err a
fromEitherPoly (Left errs) = Errors errs
fromEitherPoly (Right a) = Success a

-- | Join nested 'MultiExcept's with the same error type.
--   Note that this doesn't imply a __useful__ 'Monad' instance.
--   The instance defined in terms of join discards errors on the RHS of '>>='.
join :: MultiExcept err (MultiExcept err a) -> MultiExcept err a
join (Success a) = a
join (Errors a) = Errors a

instance Functor (MultiExcept err) where
  fmap f (Success a) = Success $ f a
  fmap _ (Errors errs) = Errors errs

instance Bifunctor MultiExcept where
 bimap _ fa (Success a)    = Success $ fa a
 bimap ferr _ (Errors err) = Errors $ fmap ferr err

instance Applicative (MultiExcept err) where
  pure = Success

  Errors l <*> Errors l' = Errors $ l <> l'
  Success f <*> Success a = Success $ f a
  Errors l <*> _ = Errors l
  _ <*> Errors l = Errors l

instance Alt (MultiExcept err) where
  Success a <!> _ = Success a
  _ <!> Success a = Success a
  Errors l <!> Errors r = Errors (l <> r)
