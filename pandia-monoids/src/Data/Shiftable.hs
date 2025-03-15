{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}

--------------------------------------------------------------------------------
-- |
--
-- Module      :  Pandia.Core.Media.Internal
-- Description :  Actions of sets, semigroups, monoids or groups.
-- Copyright   :  (c) Alice Rixte 2024
-- License     :  LGPL 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  unstable
-- Portability :  non-portable (GHC extensions)
--
-- == Presentation
--
-- Left actions of sets, semigroups, monoids or groups. An action lifts an
-- element of some type @s@, which we in this documentation we will call the
-- /acting/ type ot the /actor/ into a function of another type @x@ which we
-- call the /actee/.
--
-- == Examples
--
-- == Design choices compared with existing libraries
--
--------------------------------------------------------------------------------

module Data.Shiftable
  ( Shiftable (..)
  , LShiftable (..)
  , Origin (..)
  , fromOrigin
  ) where

import Data.Monoid as Mn
import Data.Functor.Identity
import Data.Coerce

import Data.Act
import Linear as Lin

------------------------------------ Origin ------------------------------------

class Origin s where
  origin :: s

instance Num x => Origin (Sum x) where
  origin = 0
  {-# INLINE origin #-}

instance Num x => Origin (Product x) where
  origin = 1
  {-# INLINE origin #-}

instance Origin Any where
  origin = Any False
  {-# INLINE origin #-}

instance Origin All where
  origin = All True
  {-# INLINE origin #-}

instance Origin (Mn.First a) where
  origin = Mn.First Nothing
  {-# INLINE origin #-}

instance Origin (Mn.Last a) where
  origin = Mn.Last Nothing
  {-# INLINE origin #-}


instance Origin (Maybe x) where
  origin = Nothing
  {-# INLINE origin #-}

instance Origin x => Origin (Identity x) where
  origin = Identity origin
  {-# INLINE origin #-}

instance Origin [a] where
  origin = []
  {-# INLINE origin #-}

fromOrigin :: Origin x => Maybe x -> x
fromOrigin = maybe origin id
{-# INLINE fromOrigin #-}

------------------------------------ Shift -------------------------------------

-- | A set @x@ that can be lifted in some semigroup @s@.
--
-- Instances must satisfy the following law
--
-- @lshift x <>$ origin == x@
--
-- This is a generalisation of a G-torsor
--
class (LActSg x s, Semigroup s, Origin x) => LShiftable x s where
  lshift :: x -> s

instance (Origin s, Semigroup s) => LShiftable s (ActSelf s) where
  lshift = ActSelf

instance (Semigroup s, Coercible x s, Origin x)
  => LShiftable x (ActCoerce s) where
  lshift = ActCoerce . coerce
  {-# INLINE lshift #-}

instance LShiftable x s => LShiftable (Identity x) (Identity s) where
  lshift = coerce (lshift :: x -> s)
  {-# INLINE lshift #-}



class (LAct s (Shift s), Semigroup (Shift s)) => Shiftable s where
  type Shift s
  shift :: s -> Shift s

instance Num x => Shiftable (Sum x) where
  type Shift (Sum x) = Sum x
  shift = id
  {-# INLINE shift #-}

instance Num x => Shiftable (Product x) where
  type Shift (Product x) = Product x
  shift = id
  {-# INLINE shift #-}

instance Shiftable x => Shiftable (Identity x) where
  type Shift (Identity x) = Identity (Shift x)
  shift = coerce . shift
  {-# INLINE shift #-}

instance Semigroup a => Shiftable (V1 a) where
  type Shift (V1 a) = V1 a
  shift = id
  {-# INLINE shift #-}

instance Semigroup a => Shiftable (V2 a) where
  type Shift (V2 a) = V2 a
  shift = id
  {-# INLINE shift #-}

instance Semigroup a => Shiftable (V3 a) where
  type Shift (V3 a) = V3 a
  shift = id
  {-# INLINE shift #-}

instance Semigroup a => Shiftable (V4 a) where
  type Shift (V4 a) = V4 a
  shift = id
  {-# INLINE shift #-}