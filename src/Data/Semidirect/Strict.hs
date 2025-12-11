-----------------------------------------------------------------------------
-- |
-- Module      : Data.Semidirect.Strict
-- Description : Strict semidirect products
-- Copyright   : (c) Alice Rixte 2025
-- License     : BSD 3
-- Maintainer  : alice.rixte@u-bordeaux.fr
-- Stability   : unstable
-- Portability : non-portable (GHC extensions)
--
-- Semidirect products for left and right actions.
--
-- For a lazy version, see @'Data.Semidirect.Lazy'@.
--
-- [Usage :]
--
-- >>> import Data.Semigroup
-- >>> LPair (Sum 1) (Product 2) <> LPair (Sum (3 :: Int)) (Product (4 :: Int))
-- LPair {lactee = Sum {getSum = 7}, lactor = Product {getProduct = 8}}
--
-- [Property checking :]
--
-- There is a @'Semigroup'@ instance for @'LSemidirect'@ (resp. @'RSemidirect'@)
-- only if there is a @'LActSgMorph'@ (resp. @'RActSgMorph'@) instance. For
-- example, @'Sum' Int@ acting on itself is not a semigroup action by morphism
-- and therefore the semidirect product is not associative :
--
-- >>> LPair (Sum 1) (Sum 2) <> LPair (Sum (3 :: Int)) (Sum (4 :: Int))
-- No instance for `LActDistrib (Sum Int) (Sum Int)'
--   arising from a use of `<>'
--
-----------------------------------------------------------------------------

module Data.Semidirect.Strict
      ( LSemidirect (..)
      , lvacate
      , lerase
      , lreset
      , lforget
      , lfromActor
      , lembedActee
      , lfromActee
      , lembedActor
      , lfromPair
      , RSemidirect (..)
      , rreset
      , rerase
      , rvacate
      , rforget
      , rfromActor
      , rembedActee
      , rfromActee
      , rembedActor
      , rfromPair
      ) where

import Data.Bifunctor
import Data.Act

-- | A semi-direct product for a left action, where @s@ acts on @x@
--
data LSemidirect x s = LPair
  { lactee :: !x -- ^ The value being acted on
  , lactor :: !s -- ^ The acting element
  }
  deriving (Show, Read, Eq)

instance LActSgMorph x s
  => Semigroup (LSemidirect x s) where
  LPair x s <> LPair x' s' =
    LPair  (x <> (s <>$ x')) (s <> s')

instance LActMnMorph x s => Monoid (LSemidirect x s) where
  mempty = LPair mempty mempty

instance Functor (LSemidirect x) where
  fmap f a = a {lactor = f (lactor a)}

instance Bifunctor LSemidirect where
  first f a = a {lactee = f (lactee a)}
  second = fmap

-- | Replace the actee with @mempty@.
lvacate :: Monoid x => LSemidirect x s -> LSemidirect x s
lvacate a = a {lactee = mempty}

-- | Erase the actee (i.e. replace it with @mempty@).
lerase :: Monoid x => LSemidirect x s -> LSemidirect x s
lerase a = a {lactee = mempty}

{-# DEPRECATED lerase "Use lvacate instead." #-}

-- | Replace the actor with @mempty@.
lreset :: Monoid s => LSemidirect x s -> LSemidirect x s
lreset a = a {lactor = mempty}

-- | Forget the actor (i.e. replace it with @mempty@).
lforget :: Monoid s => LSemidirect x s -> LSemidirect x s
lforget a =a {lactor = mempty}

{-# DEPRECATED lforget "Use lreset instead." #-}

-- | Make a semidirect pair whose actee is @mempty@.
lfromActor :: Monoid x => s -> LSemidirect x s
lfromActor s = LPair mempty s

-- | Make a semidirect pair whose actee is @mempty@.
lembedActor :: Monoid x => s -> LSemidirect x s
lembedActor s = LPair mempty s

{-# DEPRECATED lembedActor "Use lfromActor instead." #-}

-- | Make a semidirect pair whose actor is @mempty@.
lfromActee :: Monoid s => x -> LSemidirect x s
lfromActee x = LPair x mempty

-- | Make a semidirect pair whose actor is @mempty@.
lembedActee :: Monoid s => x -> LSemidirect x s
lembedActee x = LPair x mempty

{-# DEPRECATED lembedActee "Use lfromActee instead." #-}

-- | Convert a pair into a semidirect product element.
lfromPair :: (x,s) -> LSemidirect x s
lfromPair (x,s) = LPair x s


------------------------------------------------------------------------------

-- | A semidirect product for a right action, where @s@ acts on @x@
--
data RSemidirect x s = RPair
  { ractee :: !x -- ^ The value being acted on
  , ractor :: !s -- ^ The acting element
  }
  deriving (Show, Read, Eq)

instance RActSgMorph x s
  => Semigroup (RSemidirect x s) where
  RPair x s <> RPair x' s' =
    RPair  (x <> (x' $<> s)) (s <> s')

instance RActMnMorph x s => Monoid (RSemidirect x s) where
  mempty = RPair mempty mempty

instance Functor (RSemidirect x) where
  fmap f a = a {ractor = f (ractor a)}

instance Bifunctor RSemidirect where
  first f a = a {ractee = f (ractee a)}
  second = fmap

-- | Replace the actee with @mempty@.
rvacate :: Monoid x => RSemidirect x s -> RSemidirect x s
rvacate a = a {ractee = mempty}

-- | Erase the actee (i.e. replace it with @mempty@).
rerase :: Monoid x => RSemidirect x s -> RSemidirect x s
rerase a = a {ractee = mempty}

{-# DEPRECATED rerase "Use rvacate instead." #-}

-- | Replace the actor with @mempty@.
rreset :: Monoid s => RSemidirect x s -> RSemidirect x s
rreset a = a {ractor = mempty}

-- | Forget the actor (i.e. replace it with @mempty@).
rforget :: Monoid s => RSemidirect x s -> RSemidirect x s
rforget a = a {ractor = mempty}

{-# DEPRECATED rforget "Use rreset instead." #-}

-- | Make a semidirect pair whose actee is @mempty@.
rfromActor :: Monoid x => s -> RSemidirect x s
rfromActor s = RPair mempty s

-- | Make a semidirect pair whose actee is @mempty@.
rembedActor :: Monoid x => s -> RSemidirect x s
rembedActor s = RPair mempty s


{-# DEPRECATED rembedActor "Use rfromActor instead." #-}

-- | Make a semidirect pair whose actor is @mempty@.
rfromActee :: Monoid s => x -> RSemidirect x s
rfromActee x = RPair x mempty

-- | Make a semidirect pair whose actor element is @mempty@ .
rembedActee :: Monoid s => x -> RSemidirect x s
rembedActee x = RPair x mempty

{-# DEPRECATED rembedActee "Use rfromActee instead." #-}

-- | Convert a pair into a semidirect product element
rfromPair :: (x,s) -> RSemidirect x s
rfromPair (x,s) = RPair x s
