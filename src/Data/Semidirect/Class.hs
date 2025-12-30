-----------------------------------------------------------------------------
-- |
-- Module      : Data.Semidirect.Class
-- Description : A class for semidirect products
-- Copyright   : (c) Alice Rixte 2025
-- License     : BSD 3
-- Maintainer  : alice.rixte@u-bordeaux.fr
-- Stability   : unstable
-- Portability : non-portable (GHC extensions)
--
-- Semidirect products abstracted as a class.
--
-----------------------------------------------------------------------------

module Data.Semidirect.Class
  ( Semidirect (..)
  ) where

import Data.Bifunctor

class Bifunctor sd => Semidirect sd where
  {-# MINIMAL fromPair, toPair #-}

  -- | Converts a pair into a semidirect product.
  fromPair :: (x,s) -> sd x s

  -- |  Make a semidirect pair whose actee is @mempty@.
  fromActor :: Monoid x => s -> sd x s
  fromActor a = fromPair (mempty, a)
  {-# INLINE fromActor #-}

  -- |  Make a semidirect pair whose actor is @mempty@.
  fromActee :: Monoid s => x -> sd x s
  fromActee x = fromPair (x, mempty)
  {-# INLINE fromActee #-}

  -- | Converts a semidirect product element into a pair.
  toPair :: sd x s -> (x,s)

  -- | Extract the actor from a semidirect product.
  toActor :: sd x s -> s
  toActor = snd . toPair
  {-# INLINE toActor #-}

  -- | Extract the actee from a semidirect product.
  toActee :: sd x s -> x
  toActee = fst . toPair
  {-# INLINE toActee #-}

  -- | Replace the actee with @mempty@.
  vacate :: Monoid x => sd x s -> sd x s
  vacate = first (const mempty)
  {-# INLINE vacate #-}

  -- | Replace the actor with @mempty@.
  reset :: Monoid s => sd x s -> sd x s
  reset = second (const mempty)
  {-# INLINE reset #-}



