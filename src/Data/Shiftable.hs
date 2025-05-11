{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DefaultSignatures          #-}

--------------------------------------------------------------------------------
-- |
--
-- Module      :  Pandia.Core.Media.Internal
-- Description :  Actions of sets, semigroups, monoids or groups.
-- Copyright   :  (c) Alice Rixte 2024
-- License     :  BSD 3
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
  (
    Shift
  , Origin (..)
  , fromOrigin
  , maybeOrigin
  ) where

import Data.Monoid as Mn
import Data.Functor.Identity
import Data.Coerce
import Data.Maybe (fromMaybe)

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

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

maybeOrigin :: Origin a => Lens' (Maybe a) a
maybeOrigin f a = fmap Just (f (fromMaybe origin a))
{-# INLINE maybeOrigin #-}

------------------------------------ Shift -------------------------------------

type family Shift s



-- | A set @x@ that can be lifted in some semigroup @s@.
--
-- Instances must satisfy the following law
--
-- @lshift x <>$ origin == x@
--
-- This is a generalisation of a G-torsor
-- --
-- class (LActSg x s, Semigroup s, Origin x) => LShiftable x s where
--   lshift :: x -> s

-- instance (Origin s, Semigroup s) => LShiftable s (ActSelf s) where
--   lshift = ActSelf

-- instance (Semigroup s, Coercible x s, Origin x)
--   => LShiftable x (ActSelf' s) where
--   lshift = ActSelf' . coerce
--   {-# INLINE lshift #-}

-- instance LShiftable x s => LShiftable (Identity x) (Identity s) where
--   lshift = coerce (lshift :: x -> s)
--   {-# INLINE lshift #-}



-- class (LAct s (Shift s), Semigroup (Shift s)) => Shiftable s where
--   type Shift s
--   shift :: s -> Shift s

-- instance Num x => Shiftable (Sum x) where
--   type Shift (Sum x) = Sum x
--   shift = id
--   {-# INLINE shift #-}

-- instance Num x => Shiftable (Product x) where
--   type Shift (Product x) = Product x
--   shift = id
--   {-# INLINE shift #-}

-- instance Shiftable x => Shiftable (Identity x) where
--   type Shift (Identity x) = Identity (Shift x)
--   shift = coerce . shift
--   {-# INLINE shift #-}


------------------------------------------------------------------------------

