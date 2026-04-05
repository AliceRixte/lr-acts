{-# OPTIONS_GHC -Wno-orphans #-}

module Witch.Orphans
  () where

import qualified Data.Semigroup as Semigroup
import qualified Data.Monoid as Monoid

import Witch

-- Data.Monoid newtypes

-- | Uses @'coerce'@.
instance From a (Monoid.Dual a)

-- | Uses @'coerce'@.
instance From (Monoid.Dual a) a

-- | Uses @'coerce'@.
instance From (a -> a) (Monoid.Endo a)

-- | Uses @'coerce'@.
instance From (Monoid.Endo a) (a -> a)

-- | Uses @'coerce'@.
instance From Bool Monoid.All

-- | Uses @'coerce'@.
instance From Monoid.All Bool

-- | Uses @'coerce'@.
instance From Bool Monoid.Any

-- | Uses @'coerce'@.
instance From Monoid.Any Bool

-- | Uses @'coerce'@.
instance From a (Monoid.Sum a)

-- | Uses @'coerce'@.
instance From (Monoid.Sum a) a

-- | Uses @'coerce'@.
instance From a (Monoid.Product a)

-- | Uses @'coerce'@.
instance From (Monoid.Product a) a

-- | Uses @'coerce'@.
instance From (Maybe a) (Monoid.First a)

-- | Uses @'coerce'@.
instance From (Monoid.First a) (Maybe a)

-- | Uses @'coerce'@.
instance From (Maybe a) (Monoid.Last a)

-- | Uses @'coerce'@.
instance From (Monoid.Last a) (Maybe a)

-- | Uses @'coerce'@.
instance From (f a) (Monoid.Alt f a)

-- | Uses @'coerce'@.
instance From (Monoid.Alt f a) (f a)

-- | Uses @'coerce'@.
instance From (f a) (Monoid.Ap f a)

-- | Uses @'coerce'@.
instance From (Monoid.Ap f a) (f a)

-- Data.Semigroup newtypes

-- | Uses @'coerce'@.
instance From a (Semigroup.Min a)

-- | Uses @'coerce'@.
instance From (Semigroup.Min a) a

-- | Uses @'coerce'@.
instance From a (Semigroup.Max a)

-- | Uses @'coerce'@.
instance From (Semigroup.Max a) a

-- | Uses @'coerce'@.
instance From a (Semigroup.First a)

-- | Uses @'coerce'@.
instance From (Semigroup.First a) a

-- | Uses @'coerce'@.
instance From a (Semigroup.Last a)

-- | Uses @'coerce'@.
instance From (Semigroup.Last a) a


