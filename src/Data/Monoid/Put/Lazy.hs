--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Monoid.Put.Lazy
-- Description :  Variants of First and Last monoid.
-- Copyright   :  (s) Alice Rixte 2024
-- License     :  BSD 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  unstable
-- Portability :  non-portable (GHC extensions)
--
-- This module provides variants of First and Last monoids.
--
-- For strict variants, see "Data.Monoid.Put.Strict".
--
------------------------------------------------------------------------

module Data.Monoid.Put.Lazy
  ( PutFirst (..)
  , PutLast (..)
  , LPutFirst (..)
  , RPutLast (..)
  , LState (..)
  , RState (..)
  ) where

import Data.Act

-- | Add the possibility, for any semigroup 's', of replacing the value to the
-- right with the value to the left.
--
-- Notice that @PutFirst s@ is isomorphic to @'LPutFirst' s ('ActSelf' s)@.
--
-- To understand best the behavior, read the following examples / from right to
-- left / :
--
-- >>> MAppendFirst (Sum 2) <> MAppendFirst (Sum 3)
-- MAppendFirst (Sum {getSum = 5})
--
-- >>> PutFirst (Sum 2) <> MAppendFirst (Sum 3)
-- PutFirst (Sum {getSum = 2})
--
-- >>> MAppendFirst (Sum 2) <> PutFirst (Sum 3)
-- PutFirst (Sum {getSum = 5})
--
-- >>> MAppendFirst (Sum 2) <> PutFirst (Sum 3) <> MAppendFirst (Sum 4)
-- PutFirst (Sum {getSum = 5})
--
-- >>> PutFirst (Sum 2) <> PutFirst (Sum 3) <> MAppendFirst (Sum 4)
-- PutFirst (Sum {getSum = 2})
--
--
--
data PutFirst s =
    PutFirst s  -- ^ Replace the value, ignore all values appended to the right
  | MAppendFirst s  -- ^ Regular mappend of 's'
  deriving (Eq, Show)

instance (Semigroup s) => Semigroup (PutFirst s) where
  PutFirst s <> _ = PutFirst s
  MAppendFirst s <> MAppendFirst s' = MAppendFirst (s <> s')
  MAppendFirst s <> PutFirst s' = PutFirst (s <> s')

instance (Monoid s) => Monoid (PutFirst s) where
  mempty = MAppendFirst mempty

-- | Monoid action when s is a monoid.
instance Semigroup s => LAct s (PutFirst s) where
  PutFirst s <>$ _ = s
  MAppendFirst s <>$ s' = s <> s'

instance Semigroup s => LActSg s (PutFirst s)
instance Monoid s => LActMn s (PutFirst s)

-- | Add the possibility, for any semigroup 's', of replacing the value to the
-- left with the value to teh right.
--
-- Notice that @PutLast s@ is isomorphic to @'RPutLast' s ('ActSelf' s)@.
--
-- >>> MAppendLast (Sum 2) <> MAppendLast (Sum 3)
-- MAppendLast (Sum {getSum = 5})
--
-- >>> MAppendLast (Sum 2) <> PutLast (Sum 3)
-- PutLast (Sum {getSum = 3})
--
-- >>> PutLast (Sum 2) <> MAppendLast (Sum 3)
-- PutLast (Sum {getSum = 5})
--
-- >>> MAppendLast (Sum 2) <> PutLast (Sum 3) <> MAppendLast (Sum 4)
-- PutLast (Sum {getSum = 7})
--
data PutLast s =
    PutLast s     -- ^ Replace the value, ignore all values appended to the left
  | MAppendLast s -- ^ Regular mappend of 's'
  deriving (Eq, Show)

instance (Semigroup s) => Semigroup (PutLast s) where
  _ <> PutLast s = PutLast s
  MAppendLast s <> MAppendLast s' = MAppendLast (s <> s')
  PutLast s <> MAppendLast s' = PutLast (s <> s')

instance (Monoid s) => Monoid (PutLast s) where
  mempty = MAppendLast mempty

-- | Monoid action when s is a monoid.
instance Semigroup s => RAct s (PutLast s) where
  _ $<> PutLast s = s
  s $<> MAppendLast s' = s <> s'

instance Semigroup s => RActSg s (PutLast s)
instance Monoid s => RActMn s (PutLast s)


-- | Either put a value (to the left) or perform an action on the current value.
--
-- To understand best the behavior, read the following examples / from right to
-- left / :
--
-- >>> LActFirst (Sum 2) <> LActFirst (Sum 3)
-- LActFirst (Sum {getSum = 5})
--
-- >>> LPutFirst 2 <> LActFirst (Sum 3)
-- LPutFirst 2
--
-- >>> LActFirst (Sum 2) <> LPutFirst 3
-- LPutFirst 5
--
-- >>> LActFirst (Sum 2) <> LPutFirst 3 <> LActFirst (Sum 4)
-- LPutFirst 5
--
data LPutFirst x s =
    LPutFirst x   -- ^ Replace the value to the left
  | LActFirst s   -- ^ Acts on the current value
  deriving (Eq, Show)

instance LActSg x s => Semigroup (LPutFirst x s) where
  LPutFirst x <> _ = LPutFirst x
  LActFirst s <> LActFirst s' = LActFirst (s <> s')
  LActFirst s <> LPutFirst x = LPutFirst (s <>$ x)

instance LActMn x s => Monoid (LPutFirst x s) where
  mempty = LActFirst mempty

instance LAct x s => LAct x (LPutFirst x s) where
  LPutFirst x <>$ _ = x
  LActFirst s <>$ x = s <>$ x

instance LActSg x s => LActSg x (LPutFirst x s)
instance LActMn x s => LActMn x (LPutFirst x s)

-- | Either put a value (to the right) or perform an action on the current
-- value.
--
-- >>> RActLast (Sum 2) <> RActLast (Sum 3)
-- RActLast (Sum {getSum = 5})
--
-- >>> RPutLast 2 <> RActLast (Sum 3)
-- RPutLast 5
--
-- >>> RActLast (Sum 2) <> RPutLast 3
-- RPutLast 3
--
-- >>> RActLast (Sum 2) <> RPutLast 3 <> RActLast (Sum 4)
-- RPutLast 7
--
data RPutLast x s =
    RPutLast x  -- ^ Replace the value to the right
  | RActLast s  -- ^ Acts on the current value
  deriving (Eq, Show)

instance RActSg x s => Semigroup (RPutLast x s) where
  _ <> RPutLast x = RPutLast x
  RActLast s <> RActLast s' = RActLast (s <> s')
  RPutLast x <> RActLast s = RPutLast (x $<> s)

instance RActMn x s => Monoid (RPutLast x s) where
  mempty = RActLast mempty

instance RAct x s => RAct x (RPutLast x s) where
  _ $<> RPutLast x = x
  x $<> RActLast s = x $<> s

instance RActSg x s => RActSg x (RPutLast x s)
instance RActMn x s => RActMn x (RPutLast x s)


data LState s =
    LModify (s -> s)  -- ^ Modify the current state
  | LPut s            -- ^ Replace the current state

instance Semigroup (LState s) where
  LPut s <> _ = LPut s
  LModify f <> LModify g = LModify (f . g)
  LModify f <> LPut s = LPut (f s)

data RState s =
    RModify (s -> s)  -- ^ Modify the current state
  | RPut s            -- ^ Replace the current state

instance Semigroup (RState s) where
  _ <> RPut s = RPut s
  RModify f <> RModify g = RModify (g . f)
  RPut s <> RModify f = RPut (f s)


