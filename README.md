# multi-except

[![Hackage version](https://img.shields.io/hackage/v/multi-except?color=purple)](https://hackage.haskell.org/package/multi-except)
[![CI Status](https://img.shields.io/github/actions/workflow/status/414owen/multi-except/haskell-ci.yml)](https://github.com/414owen/multi-except/actions)
[![GitHub License](https://img.shields.io/github/license/414owen/multi-except)](https://github.com/414owen/multi-except/blob/master/LICENSE)

multi-except - succeed, or return one or more errors

## Adding the dependency

```
-- in your cabal file
  -- Add the main package (only depends on base!)
  , multi-except
  -- For the Alt instance (depends on semigroupoids)
  , multi-except:semigroupoid-instances
```

## Usage


```haskell
{-# LANGUAGE ApplicativeDo #-}

import Control.Applicative.MultiExcept

errors :: MultiExcept String (Int, Int, Int)
errors = do
  a <- throwError "no monad instance"
  b <- pure 12
  c <- throwError "i am scared"
  pure (a, b, c)

-- errors: Errors ["no monad instance", "i am scared"]
```

The use of `ApplicativeDo` is significant and necessary for using
`MultiExcept` with do notation.

`MultiExcept` is not a `Monad`, only an `Applicative`, so a few constraints
apply, such as not being able to determine the structure of the rest of the
computation based on a previously do-bound value. If the previous sentence was
confusing, then you might want to consider using a writer monad instead.

To compose with other applicative effects, you can use
[`Data.Functor.Compose`](https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Functor-Compose.html).
