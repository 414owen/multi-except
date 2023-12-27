{-|
Module      : Control.Applicative.MultiExcept
Copyright   : (c) Owen Shepherd, 2021
License     : MIT
Maintainer  : owen@owen.cafe
Stability   : stable
Portability : portable
-}

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Applicative.MultiExcept
  ( MultiExcept
  , fromEither
  , fromEitherPoly
  , join
  , or
  , runMultiExcept
  , succeed
  , throwError
  , throwErrors
  , mapMultiExcept
  ) where

import Prelude (Eq(..), Ord(..), Either(..), (.), ($), id, Show(..), (++))

import Control.Applicative  (Applicative(..))
#if MIN_VERSION_base(4,8,0)
import Data.Bifunctor
#endif
import Data.Functor         (Functor(..), (<$>))
import Data.Foldable        (Foldable(..))
import Data.Traversable     (Traversable(..))
#if MIN_VERSION_base(4,9,0)
import Data.List.NonEmpty   (NonEmpty(..))
#endif


-- NonEmptyDList

-- | This is written here because:
-- * The version in dlist is currently limited to ghc>=8.0
-- * The version in dlist-nonempty is too heavy on dependencies
-- * We only need a few trivial features anyway

data NonEmptyDList a = NonEmptyDList !a !([a] -> [a])

{-# INLINE nedlSingleton #-}
nedlSingleton :: a -> NonEmptyDList a
nedlSingleton a = NonEmptyDList a id

{-# INLINE runNonEmptyDList #-}
runNonEmptyDList :: NonEmptyDList a -> (a, [a])
runNonEmptyDList (NonEmptyDList x xs) = (x, xs [])

instance Eq a => Eq (NonEmptyDList a) where
  NonEmptyDList x xs == NonEmptyDList y ys = x : xs [] == y : ys []

instance Ord a => Ord (NonEmptyDList a) where
  NonEmptyDList x xs `compare` NonEmptyDList y ys = (x : xs []) `compare` (y : ys [])

instance Show a => Show (NonEmptyDList a) where
  show nedl = case runNonEmptyDList nedl of
    (x, xs) -> show $ x : xs

appendNedl :: NonEmptyDList a -> NonEmptyDList a -> NonEmptyDList a
appendNedl (NonEmptyDList x xs) (NonEmptyDList y ys) = NonEmptyDList x $ xs . (y:) . ys

-- WARNING: O(n) space
-- TODO Make this constant space
instance Functor NonEmptyDList where
  fmap f nedl = case runNonEmptyDList nedl of
    (x, xs) -> NonEmptyDList (f x) (fmap f xs ++)

-- | A 'MultiExcept' is a success value, or one or more errors.
data MultiExcept err a
  = Success !a
  | Errors !(NonEmptyDList err)
  deriving (Eq, Ord, Show)

-- | Run the computation.
runMultiExcept :: MultiExcept err a -> Either (NonEmptyDList err) a
runMultiExcept (Errors errs) = Left errs
runMultiExcept (Success a) = Right a

-- | Throw a single error.
throwError :: forall a err. err -> MultiExcept err a
throwError = Errors . nedlSingleton

#if MIN_VERSION_base(4,9,0)

-- | Throw one or more errors.
throwErrors :: forall a err. NonEmpty err -> MultiExcept err a
throwErrors (err :| errs) = Errors $ NonEmptyDList err (errs ++)

#else

-- | Throw one or more errors.
throwErrors :: forall a err. (err, [err]) -> MultiExcept err a
throwErrors (err, errs) = Errors $ NonEmptyDList err (errs ++)

#endif

-- | Embeds a value into a 'MultiExcept' context.
succeed :: forall err a. a -> MultiExcept err a
succeed = Success

-- | Convert an 'Either' to a 'MultiExcept'.
fromEither :: Either err a -> MultiExcept err a
fromEither (Left err) = throwError err
fromEither (Right a) = Success a

#if MIN_VERSION_base(4,9,0)

-- | Convert a multi-error 'Either' to a 'MultiExcept'.
fromEitherPoly :: Either (NonEmpty err) a -> MultiExcept err a
fromEitherPoly (Left errs) = throwErrors errs
fromEitherPoly (Right a) = Success a

#else

-- | Convert a multi-error 'Either' to a 'MultiExcept'.
fromEitherPoly :: Either (err, [err]) a -> MultiExcept err a
fromEitherPoly (Left errs) = throwErrors errs
fromEitherPoly (Right a) = Success a

#endif

-- | Join nested 'MultiExcept's with the same error type.
--   Note that this doesn't imply a __useful__ 'Monad' instance.
--   The instance defined in terms of join discards errors on the RHS of '>>='.
join :: MultiExcept err (MultiExcept err a) -> MultiExcept err a
join (Success a) = a
join (Errors a) = Errors a

instance Functor (MultiExcept err) where
  fmap f (Success a) = Success $ f a
  fmap _ (Errors errs) = Errors errs

mapMultiExcept:: (err -> err') -> (a -> a') -> MultiExcept err a -> MultiExcept err' a'
mapMultiExcept _ fa (Success a)    = Success $ fa a
mapMultiExcept ferr _ (Errors err) = Errors $ fmap ferr err


#if MIN_VERSION_base(4,8,0)

-- | WARNING: O(n) space and time in the length of the amount of errors
-- this could be fixed by changing the difference list Functor instance.
instance Bifunctor MultiExcept where
  bimap = mapMultiExcept

#endif

instance Applicative (MultiExcept err) where
  pure = Success

  Errors l <*> Errors l' = Errors $ appendNedl l l'
  Success f <*> Success a = Success $ f a
  Errors l <*> _ = Errors l
  _ <*> Errors l = Errors l

or :: MultiExcept err a -> MultiExcept err a -> MultiExcept err a
Success a `or` _ = Success a
_ `or` Success a = Success a
Errors l `or` Errors r = Errors $ appendNedl l r

instance Foldable (MultiExcept err) where
  foldr f acc (Success a) = f a acc
  foldr _ acc _           = acc

instance Traversable (MultiExcept err) where
  traverse f (Success a)   = Success <$> f a
  traverse _ (Errors err) = pure $ Errors err
