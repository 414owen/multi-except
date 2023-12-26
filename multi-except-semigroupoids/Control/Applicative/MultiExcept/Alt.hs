{-|
Module      : Control.Applicative.MultiExcept.Alt
Copyright   : (c) Owen Shepherd, 2023
License     : MIT
Maintainer  : owen@owen.cafe
Stability   : stable
Portability : portable
-}

{-# LANGUAGE CPP #-}
#if MIN_VERSION_base(4,9,0)
{-# OPTIONS_GHC -Wno-orphans #-}
#endif

module Control.Applicative.MultiExcept.Alt where

import Prelude ()
import Control.Applicative.MultiExcept
import Data.Functor.Alt                (Alt(..))

instance Alt (MultiExcept err) where
  (<!>) = or
