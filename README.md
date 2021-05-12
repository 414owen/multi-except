# multi-except

[![hackage-version](https://img.shields.io/hackage/v/multi-except?color=purple)](https://hackage.haskell.org/package/multi-except-0.1.1.0)

multi-except - succeed, or return one or more errors

## Examples:

```haskell
{-# LANGUAGE ApplicativeDo #-}

import Data.List.NonEmpty (NonEmpty)
import Control.Applicative.MultiExcept

errors :: MultiExcept String (Int, Int)
errors = do
  a <- throwError "no monad instance"
  b <- throwError "i am scared"
  return (a, b)

-- errors: Errors ["no monad instance", "i am scared"]
```
