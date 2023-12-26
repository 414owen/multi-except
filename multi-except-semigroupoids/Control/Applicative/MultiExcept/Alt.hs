{-|
Module      : Control.Applicative.MultiExcept.Alt
Copyright   : (c) Owen Shepherd, 2023
License     : MIT
Maintainer  : owen@owen.cafe
Stability   : stable
Portability : portable
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Applicative.MultiExcept.Alt where

import Prelude ()
import Control.Applicative.MultiExcept
import Data.Functor.Alt                (Alt(..))

instance Alt (MultiExcept err) where
  (<!>) = or
