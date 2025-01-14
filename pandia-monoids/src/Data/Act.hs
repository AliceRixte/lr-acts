{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Act
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

module Data.Act
  ( LAct (..)
  , ActSelf (..)
  , ActId (..)
  , ActMap (..)
  , ActFold (..)
  , ActCoerce (..)
) where

import Data.Semigroup
import Data.Functor.Identity

import Data.Coerce

import Linear


-- | A left action of a set @s@ on another set @x@ is a function that maps
-- elements of @s@ to functions on @x@.
--
-- There are no additional laws for this class to satisfy. This is because there
-- are too many action properties and specify them explicitly via a type classes
-- quickly becomes a burden. However, every LAct instance should come with a
-- comment expliciting which property it satisfied. More on this in this
-- module's introduction.
--
-- The order @'LAct'@'s arguments is counter intuitive : even though we write
-- left actions as @s <>$ x@, we declare the constraint as @LAct x s@. The
-- reason for this is to be able to derive instances of @LAct@ while driving the
-- instances by the acting type. See the comparison with the @acts@ library for
-- more explanations on this design choice.
--
-- Instances of @LAct@ are driven by the second parameter (the acting type).
-- Concretely, this means you should never write instances of the form
--
-- @instance LAct s SomeType@
--
-- where @s@ is a type variable.
--
-- If you need such an instance, you should make a newtype. This library already
-- provides some, such as @'ActSelf'@,  @'ActId'@, @'ActCoerce'@, @'ActFold'@
-- and @'ActMap'@.
--
class LAct x s where
  -- | Lifts an element of the set @s@ into a function on the set @x@
  lact :: s -> x -> x
  lact = (<>$)
  infixr 7 `lact`

  -- | Infix synonym or @'lact'@
  (<>$) :: s -> x -> x
  (<>$) = lact
  infixr 7 <>$

  {-# MINIMAL lact | (<>$) #-}

------------------------------- Newtype actions --------------------------------

-- | A semigroup always acts on itself by translation.
--
-- Notice that whenever there is an instance @LAct x s@ with @x@ different from
-- @s@, this action is lifted to an @ActSelf@ action.
newtype ActSelf s = ActSelf {unactSelf :: s}
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

-- | Semigroup action (monoid action when @Monoid s@)
instance Semigroup s => LAct s (ActSelf s) where
  ActSelf s <>$ x = s <> x
  {-# INLINE (<>$) #-}

-- | Semigroup action (monoid action when @Monoid s@)
instance {-# OVERLAPPABLE #-} LAct x s => LAct x (ActSelf s) where
  ActSelf s <>$ x = s <>$ x
  {-# INLINE (<>$) #-}

-- | Actions of @ActCoerce@ behave similarly to those of @'ActSelf'@, but first
-- try to coerce @x@ to @s@ before using the @Semigroup@ instance. If @x@ can be
-- coerced to @s@, then we use the @ActSelf@ action.
--
-- This is meant to be used in conjunction with the @deriving via@ strategy when
-- defining newtype wrappers. Here is a concrete example, where durations act on
-- time. Here, @Time@ is not a semigroup and @Duration@ is a group that acts on
-- time via the derived instance @LAct Time Duration@.
--
-- @
-- import Data.Semigroup
--
-- newtype Time = Time Float
--
-- newtype Duration = Duration Time
-- deriving ('Semigroup', 'Monoid', 'Group') via ('Sum' Float)
-- deriving ('LAct' Time) via ('ActCoerce' ('Sum' Float))
-- @
newtype ActCoerce x = ActCoerce {unactCoerce :: x}
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

-- | Semigroup action (monoid action when @Monoid s@)
instance {-# OVERLAPPABLE #-} (Semigroup s, Coercible x s)
  => LAct x (ActCoerce s) where
  ActCoerce s <>$ x = coerce $ s <> (coerce x :: s)
  {-# INLINE (<>$) #-}

-- | The trivial action where any element of @s@ acts as the identity function
-- on @x@
newtype ActId x = ActId  {unactId :: x}
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

-- | Action by morphism of monoids  when @'Monoid' s@ and @'Monoid' x@
instance LAct x (ActId s) where
  (<>$) _ = id
  {-# INLINE (<>$) #-}

-- | An action on any functor that uses the @fmap@ function. For example :
--
-- @'ActMap' ('ActSelf' \"Hello\") <>$ [\" World\", " !"]  === ["Hello World", "Hello !"]@
newtype ActMap s = ActMap {unactMap :: s}
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

-- | Preserves the semigroup (resp. monoid) property of @'LAct' x s@, but
-- __not__ the morphism properties, which depend on potential @'Semigroup'@
-- (resp. @'Monoid'@) instances of @f x@
instance (LAct x s, Functor f) => LAct (f x) (ActMap s) where
  ActMap s <>$ x = fmap (s <>$) x
  {-# INLINE (<>$) #-}

-- | An action @(<>$)@ can be feeded as an operator for the @'foldr'@ function,
-- allowing to lift any action to some @'Foldable'@ container. For example :
--
--  @'ActFold' ['Sum' (1 :: Int), 'Sum' 2, 'Sum' 3] <>$ (4 :: Int) === 10 :: Int @
newtype ActFold s = ActFold {unactFold :: s}
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

-- | When used with lists @[]@, this is a monoid action
instance (Foldable f, LAct x s) => LAct x (ActFold (f s)) where
  ActFold f <>$ x = foldr (<>$) x f
  {-# INLINE (<>$) #-}

---------------------------------- Instances -----------------------------------

-- | Monoid action
instance LAct s () where
  () <>$ x = x
  {-# INLINE (<>$) #-}

-- |  Action by morphism of semigroups (resp. monoids) when @'Semigroup' s@
-- (resp. @'Monoid' s@)
instance {-# INCOHERENT #-} LAct () s where
  _ <>$ () = ()
  {-# INLINE (<>$) #-}

-- | Monoid action when @'LAct' x s@ is a semigroup action.
instance LAct x s => LAct x (Maybe s) where
  Nothing <>$ x = x
  Just s <>$ x = s <>$ x

-- | Same action propety as the weaker properties of @('LAct' x1 s1, 'LAct' x2
-- s2)@
instance (LAct x1 s1, LAct x2 s2) => LAct (x1, x2) (s1, s2) where
  (s1, s2) <>$ (x1, x2) = (s1 <>$ x1, s2 <>$ x2)

-- | Same action propety as the weaker property of @('LAct' x s, 'LAct' x t)@
instance (LAct x s, LAct x t) => LAct x (Either s t) where
  (Left  s) <>$ x = s <>$ x
  (Right s) <>$ x = s <>$ x
-------------------- Instances for base library functors ---------------------

instance LAct x s => LAct (Identity x) (Identity s) where
  Identity s <>$ Identity x = Identity (s <>$ x)

------------------------- Instances for Data.Semigroup -------------------------

-- | Monoid action
instance Num x => LAct x (Sum x) where
  (<>$) s = coerce (s <>)
  {-# INLINE (<>$) #-}

-- | Monoid action
instance Num x => LAct x (Product x) where
  (<>$) s = coerce (s <>)
  {-# INLINE (<>$) #-}

-- | Monoid action
instance {-# OVERLAPPING #-} Num x => LAct (Sum x) (Sum x) where
  (<>$) s = coerce (s <>)
  {-# INLINE (<>$) #-}

-- | Monoid action
instance {-# OVERLAPPING #-}  Num x => LAct (Product x) (Product x) where
  (<>$) s = coerce (s <>)
  {-# INLINE (<>$) #-}

-- | Action by morphism of monoids
instance Num x => LAct (Sum x) (Product x) where
  (<>$) s = coerce (s <>)
  {-# INLINE (<>$) #-}

-- | Monoid action
instance LAct Bool Any where
  (<>$) s = coerce (s <>)
  {-# INLINE (<>$) #-}

-- | Monoid action
instance LAct Bool All where
  (<>$) s = coerce (s <>)
  {-# INLINE (<>$) #-}

-- | Monoid action
instance LAct x (First x) where
  (<>$) s = coerce (s <>)
  {-# INLINE (<>$) #-}


---------------------------- Instances for Linear ----------------------------

-- | Action by morphism of monoids
instance Semigroup a => LAct (V1 a) (V1 a) where
  (<>$) = (<>)
  {-# INLINE (<>$) #-}

-- | Action by morphism of monoids
instance Semigroup a => LAct (V2 a) (V2 a) where
  (<>$) = (<>)
  {-# INLINE (<>$) #-}

-- | Action by morphism of monoids
instance Semigroup a => LAct (V3 a) (V3 a) where
  (<>$) = (<>)
  {-# INLINE (<>$) #-}

-- | Action by morphism of monoids
instance Semigroup a => LAct (V4 a) (V4 a) where
  (<>$) = (<>)
  {-# INLINE (<>$) #-}