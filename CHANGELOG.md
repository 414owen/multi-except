# Revision history for multi-except

## Unreleased

* Split into two libraries, `multi-except`, and `multi-except:semigroupoid-instances`
* Migrated to custom dlist, removing the dependency
* Support more GHC versions (tested down to 7.0.4)

## 0.3.0.0 -- 2021-06-06

* Switch to dlist-nonempty package

## 0.2.1.0 -- 2021-05-31

* Added Bifunctor instance
* Added Foldable instance
* Added Traversable instance

## 0.2.0.0 -- 2021-05-29

* Changed order of type variables in `succeed`
* Now required GHC >= 6.8.1 due to ScopedTypeVariables usage

## 0.1.4.0 -- 2021-05-26

* Added join, fromEitherPoly, throwErrors

## 0.1.3.0 -- 2021-05-26

* Exposed fromEither

## 0.1.2.0 -- 2021-05-26

* Added fromEither

## 0.1.1.0 -- 2021-05-11

* Added Alt instance from semigroupoids

## 0.1.0.0 -- 2021-05-11

* First version. Released on an unsuspecting world.
