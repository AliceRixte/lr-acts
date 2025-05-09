{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}

--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Act
-- Description :  Group torsors for left and right actions.
-- Copyright   :  (c) Alice Rixte 2025
-- License     :  LGPL 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  unstable
-- Portability :  non-portable (GHC extensions)
--
-- == Presentation
--
--
--------------------------------------------------------------------------------

module Data.Act.Torsor
  ( LTorsor (..)
  , RTorsor (..)
  )
where

import Data.Monoid

import Data.Act

-- | A left group torsor.
--
-- The most well known example of a torsor is the particular case of an affine
-- space where the group is the additive group of the vector space and the set
-- is a set of points. Torsors are more general than affine spaces since they
-- don't enforce linearity.
--
-- See this nLab article for more information :
-- https://ncatlab.org/nlab/show/torsor
--
-- [In algebraic terms : ]
--
-- A left group action is a torsor if and only if for every pair @(x,y) :: (x,
-- x)@, there exist a unique group element @g :: g@ such that @g <>$ x = y@.
--
-- [In Haskell terms : ]
--
-- Instances must satisfy the following law :
--
-- * @ y .-. x <>$ x == @ @y@
-- * if @g <>$ x == y@ then @g == y .-. x@
--
class LActGp x g => LTorsor x g where
  {-# MINIMAL ldiff | (.-.) #-}
  -- | @ldiff y x@ is the only group element such that @'ldiff' y x <>$ x = y@.
  ldiff :: x -> x -> g
  ldiff y x = y .-. x
  infix 6 `ldiff`
  {-# INLINE ldiff #-}

  -- | Infix synonym for 'ldiff'.
  --
  -- This represents a point minus a point.
  --
  (.-.) :: LTorsor x g => x -> x -> g
  (.-.) = ldiff
  infix 6 .-.
  {-# INLINE (.-.) #-}

instance Num x => LTorsor x (Sum x) where
  ldiff y x = Sum (y - x)

instance Fractional x => LTorsor x (Product x) where
  ldiff y x = Product (y / x)


-- | A right group torsor.
--
-- [In algebraic terms : ]
--
-- A left group action is a torsor if and only if for every pair @(x,y) :: (x,
-- x)@, there exist a unique group element @g :: g@ such that @g <>$ x = y@.
--
-- [In Haskell terms : ]
--
-- Instances must satisfy the following law :
--
-- * @ x $<> y .~. x == @ @y@
-- * if @x $<> g == y@ then @g == y .~. x@
--
class RActGp x g => RTorsor x g where
  {-# MINIMAL rdiff | (.~.) #-}
  -- | @rdiff y x@ is the only group element such that @'rdiff' y x $<> x = y@.
  rdiff :: x -> x -> g
  rdiff y x = y .~. x
  infix 6 `rdiff`
  {-# INLINE rdiff #-}

  -- | Infix synonym for 'rdiff'.
  --
  -- This represents a point minus a point.
  --
  (.~.) :: RTorsor x g => x -> x -> g
  (.~.) = rdiff
  infix 6 .~.
  {-# INLINE (.~.) #-}

instance Num x => RTorsor x (Sum x) where
  rdiff y x = Sum (y - x)

instance Fractional x => RTorsor x (Product x) where
  rdiff y x = Product (y / x)

