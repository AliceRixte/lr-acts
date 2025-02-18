module Data.Semigroup.Zero
  ( module Data.Semigroup.Zero
  ) where

-- | Semigroup with a zero element.
--
-- Instances must satisfy the following law :
--
-- @ 'zero' '<>' a = zero = a '<>' 'zero' @
class Semigroup a => HasZero a where
  -- | The zero element of a semigroup
  zero :: a

newtype LeftZero a = LeftZero a
  deriving ( Show, Eq, Ord, Num, Fractional
           , Floating, Real, RealFrac, RealFloat, Bounded, Enum)

instance Semigroup (LeftZero a) where
  LeftZero a <> _ = LeftZero a

newtype RightZero a = RightZero a
  deriving ( Show, Eq, Ord, Num, Fractional
           , Floating, Real, RealFrac, RealFloat, Bounded, Enum)

instance Semigroup (RightZero a) where
  _ <> RightZero a = RightZero a