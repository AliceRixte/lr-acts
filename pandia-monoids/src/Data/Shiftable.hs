{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

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
  , Origin (..)
  ) where

import Data.Semigroup
import Data.Functor.Identity
import Data.Coerce

import Data.Act

------------------------------------ Origin ------------------------------------

class Origin s where
  origin :: s

instance Num x => Origin (Sum x) where
  origin = 0
  {-# INLINE origin #-}

instance Num x => Origin (Product x) where
  origin = 1
  {-# INLINE origin #-}

instance Origin (Maybe x) where
  origin = Nothing
  {-# INLINE origin #-}

instance Origin x => Origin (Identity x) where
  origin = Identity origin
  {-# INLINE origin #-}

------------------------------------ Shift -------------------------------------
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
