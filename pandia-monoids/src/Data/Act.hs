{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ConstraintKinds            #-}

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
-- Left actions of sets, semigroups, monoids. An action lifts an element of some
-- type @s@, which we in this documentation we will call the /acting/ type ot
-- the /actor/ into a function of another type @x@ which we call the /actee/.
--
-- == Deriving LAct and RAct instances
--
-- For both @'LAct'@ and @'RAct'@, the acting type is the second parameter. This
-- is a bit counter intuitive when using @'LAct'@, but it allows to use the
-- @DerivingVia@ mechanism to derive instances of @'LAct'@ and @'RAct'@ for
-- newtypes that wrap the acting type. For example, you can use @'ActCoerce'@ as
-- follow to derive instances for @'LAct'@ and @'RAct'@ :
--
-- @
-- {-# LANGUAGE DerivingVia #-}
--
-- import Data.Act import Data.Semigroup
--
-- newtype Seconds = Seconds Float
--
-- newtype Duration = Duration Seconds
--   deriving ('Semigroup', 'Monoid') via ('Sum' Float)
--   deriving ('LAct' Seconds, 'RAct' Seconds) via ('ActCoerce' ('Sum' Float))
-- @
--
-- @
-- >>> Duration (Seconds 1) <>$ (Seconds 2)
-- Seconds 3.0
-- @
--
-- == Instances driven by the acting type
--
-- The action classes do not have functional dependencies, which makes it pretty
-- awkward to work with them. To avoid overlapping issues, this library chooses
-- to drive instances by the second parameter, i.e. to never write instances of
-- the form
--
-- @ instance LAct SomeType s instance RAct SomeType s @
--
-- If you need such an instance, you should make a newtype. This library already
-- provides some, such as @'ActSelf'@,  @'ActId'@, @'ActCoerce'@, @'ActFold'@
-- and @'ActMap'@.
--
-- == Design choices compared with existing libraries
--
-- This library is inspired by the already existing action libraries.
--
-- * The deriving mechanism is inspired by the one from the @acts@ library. The
--   main difference between this library and the @acts@ library is that  @acts@
--   drives its instances by the actee parameter.
--
-- * The @monoid-extras@ library drives its instances by the acting type, but
--   does not provide a deriving mechanism. This library started as an extension
--   of @monoid-extras@, but the design choices made it diverge from it.
--
-- * The idea specifying action properties using empty classes comes from the
--   @semigroups-actions@ library, which inspired some design of this library.
--   This library offers everything @semigroups-actions@ offers, and more.
--
--------------------------------------------------------------------------------

module Data.Act
  ( LAct (..)
  , LActSg
  , LActMn
  , LActDistrib
  , LActSgMorph
  , LActNeutral
  , LActMnMorph
  -- , LActMnFree (..)
  , RAct (..)
  , RActSg
  , RActMn
  , RActDistrib
  , RActSgMorph
  , RActNeutral
  , RActMnMorph
  , ActSelf (..)
  , ActId (..)
  , ActMap (..)
  , ActFold (..)
  , ActCoerce (..)
) where

import Data.Proxy
import Data.Semigroup as S
import Data.Monoid as M
import Data.Functor.Identity
import Data.Foldable
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
-- @instance LAct SomeType s@
--
-- where @s@ is a type variable.
--

--
class LAct x s where
  {-# MINIMAL lact | (<>$) #-}
  -- | Lifts an element of the set @s@ into a function on the set @x@
  lact :: s -> x -> x
  lact = (<>$)
  {-# INLINE lact #-}
  infixr 7 `lact`

  -- | Infix synonym or @'lact'@
  (<>$) :: s -> x -> x
  (<>$) = lact
  {-# INLINE (<>$) #-}
  infixr 7 <>$

-- | A left semigroup action
--
-- Instances must satisfy the following law :
--
-- @ (s <> t) <>$ x == s <>$ (t <>$ x) @
--
class (LAct x s, Semigroup s) => LActSg x s

-- | A left monoid action, also called a left /unitary/ action.
--
-- In addition to the laws of @'LActSg'@, instances must satisfy the following
-- law :
--
-- @ 'mempty' <>$ x == x @
--
class (LActSg x s, Monoid s) => LActMn x s


-- | A left distributive action
--
-- Instances must satisfy the following law :
--
-- @ s <>$ (x <> y) == (s <>$ x) <> (s <>$ y) @
--
class (LAct x s, Semigroup x) => LActDistrib x s

-- | A left action by morphism of semigroups
--
-- Whenever the constaints @'LActSg' x s@ and @'LActDistrib' x s@ are satisfied,
-- @(s <>$)@ is a morphism of semigroups for any @s@.
--
type LActSgMorph x s =  (LActSg x s, LActDistrib x s)



-- | A left action on a monoid that preserves its neutral element.
--
-- Instances must satisfy the following law :
--
-- @ s <>$ 'mempty' == 'mempty' @
--
class (LAct x s, Monoid x) => LActNeutral x s

-- | A left action generated by a subset @u@ of @x@.
--
-- Intuitively, by acting repeteadly on the elements of @u@ with actions of @s@,
-- we can reach any element of @x@.
--
-- Since the subset @u@ maybe infinite, we give two alternative ways to define
-- it : one using a characteristic function @'lgenerators'@ and the other using
-- a list @'lgeneratorsList'@.
--
-- In algebraic terms, a subset @u@ of the set @x@ is a /generating set/ of the
-- action @LAct x s@ if for every @x :: x@, there exists a pair @(u,s) ::
-- (u,s)@. When the set @u@ is finite, the action @LAct x s@ is said to be
-- finitely generated. When @u@ is a singleton, the action is said to be
-- /cyclic/ (see @'LActCyclic'@).
--
-- See /Monoids, Acts and Categories/ by Mati Kilp, Ulrich Knauer, Alexander V.
-- Mikhalev, definition 1.5.1, p.63.
--
-- In Haskell terms, instances must satisfy the following law :
--
--
--
class LActGen x s where
  -- | The set of origins of the action @LAct x s@.
  --
  -- This is a subset of @x@, represented as its characteristic function,
  -- meaning the function that returns @True@ for all elements of @x@ that are
  -- origins of the action and @False@ otherwise.
  --
  lgenerators :: Eq x => proxy s -> x -> Bool
  lgenerators _ x = x `elem` lgeneratorsList (Proxy :: Proxy s)

  -- | The set of origins of the action @LAct x s@ seen as a list.
  --
  -- You can let this function undefined if the set of origins cannot be
  -- represented as a list.
  --
  lgeneratorsList :: proxy s -> [x]

  -- | Returns a point's associated origin @o@ along with an action @s@ such
  -- that @s <>$ o == x@.
  lshiftFromOrigin :: x -> (x,s)



-- | A free left monoid action
--
-- In mathematical terms, a set of elements @b@ of elements of type @x@ is said
-- to be a basis of the action @LAct x s@ if for every @x::x@, there is a unique
-- pair @(b,s)@ such that @s <>$ b == x@.
--
-- See /Monoids, Acts and Categories/ by Mati Kilp, Ulrich Knauer, Alexander V.
-- Mikhalev, definition 1.5.11, p.67.
--
-- In Haskell terms, instances must satisfy the following law :
--
--
-- 1. 'lbasisMember' @ (Proxy :: Proxy s) ('fst' $ 'lshiftFrom' x) == True@
-- 2. 'snd' @('lshiftFrom' x) <>$ 'fst' ('lshiftFrom' x) == x@
-- 3. if @y == s <>$ x@ and @'lbasisMember' (Proxy :: Proxy s) x == True@, then
--   @'lshiftFrom' y == (x,s)@è²
--
-- In the previous laws, the first law garanties that @b@ is indeed a member of
-- the basis, the second law ensures the equality in the decomposition and the
-- third law the uniqueness of that decomposition.
--

--
class LActMn x s => LActMnGen x s where
  lbasisMember :: proxy s -> x -> Bool
  lbasis :: proxy s -> [x]
  lshiftFromBasis :: x -> (x,s)


-- class LActFiniteFree x s => OneFree x s where
--   origin :: poxy s -> x





-- | A left action by morphism of monoids i.e. such that @(s <>$)@ is a morphism of monoids.
--
-- This is equivalent to satisfy the three following properties :
--
-- 1. left action by morphism of semigroups (i.e. @'LActSgMorph' x s@)
-- 2. left monoid action (i.e. @'LActMn' x s@)
-- 3. preseving neutral element (i.e. @'LActNeutral' x s@)
--
type LActMnMorph x s = (LActMn x s, LActSgMorph x s, LActNeutral x s)


-- | A right action of a set @s@ on another set @x@.
--
class RAct x s where
  {-# MINIMAL ract | ($<>) #-}
  -- | Act on the right of some element of @x@
  ract :: x -> s -> x
  ract = ($<>)
  {-# INLINE ract #-}
  infixl 7 `ract`

  -- | Infix synonym or @'ract'@
  ($<>) :: x -> s -> x
  ($<>) = ract
  {-# INLINE ($<>) #-}
  infixl 7 $<>


-- | A right semigroup action
--
-- Instances must satisfy the following law :
--
-- @ x $<> (s <> t) == (x $<> s) $<> t @
--
class (RAct x s, Semigroup s) => RActSg x s

-- | A right monoid action, also called a right /unitary/ action.
--
-- In addition to the laws of @'RActSg'@, instances must satisfy the following
-- law :
--
-- @ x $<> 'mempty' == x @
--
class (RActSg x s, Monoid s) => RActMn x s

-- | A right distributive action
--
-- Instances must satisfy the following law :
--
-- @ (x <> y) $<> s == (x $<> s) <> (y $<> s) @
--
class (RAct x s, Semigroup x) => RActDistrib x s


-- | A right action by morphism of semigroups
--
-- Whenever the constaints @'RActSg' x s@ and @'RActDistrib' x s@ are satisfied,
-- @($<> s)@ is a morphism of semigroups for any @s@.
--
type RActSgMorph x s =  (RActSg x s, RActDistrib x s)


-- | A right action on a monoid that preserves its neutral element.
--
-- Instances must satisfy the following law :
--
-- @ x $<> mempty == x @
--
class (RAct x s, Monoid x) => RActNeutral x s

-- | A right action by morphism of monoids i.e. such that
--
-- @($<> s)@ is a morphism of monoids
--
type RActMnMorph x s = (RActMn x s, RActSgMorph x s, RActNeutral x s)




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

instance Semigroup s => LActSg s (ActSelf s)
instance Monoid s => LActMn s (ActSelf s)

-- | Semigroup action (monoid action when @Monoid s@)
instance Semigroup s => RAct s (ActSelf s) where
  x $<> ActSelf s = x <> s
  {-# INLINE ($<>) #-}

instance Semigroup s => RActSg s (ActSelf s)
instance Monoid s => RActMn s (ActSelf s)

-- | Actions of @ActCoerce@ behave similarly to those of @'ActSelf'@, but first
-- try to coerce @x@ to @s@ before using the @Semigroup@ instance. If @x@ can be
-- coerced to @s@, then we use the @ActSelf@ action.
--
-- This is meant to be used in conjunction with the @deriving via@ strategy when
-- defining newtype wrappers. Here is a concrete example, where durations act on
-- time. Here, @Seconds@ is not a semigroup and @Duration@ is a group that acts on
-- time via the derived instance @LAct Seconds Duration@.
--
-- @
-- import Data.Semigroup
--
-- newtype Seconds = Seconds Float
--
-- newtype Duration = Duration Seconds
--   deriving ('Semigroup', 'Monoid', 'Group') via ('Sum' Float)
--   deriving ('LAct' Seconds) via ('ActCoerce' ('Sum' Float))
-- @
newtype ActCoerce x = ActCoerce {unactCoerce :: x}
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

-- | Semigroup action (monoid action when @Monoid s@)
instance {-# OVERLAPPABLE #-} (Semigroup s, Coercible x s)
  => LAct x (ActCoerce s) where
  ActCoerce s <>$ x = coerce $ s <> (coerce x :: s)
  {-# INLINE (<>$) #-}

instance (Coercible x s, Semigroup s) => LActSg x (ActCoerce s)
instance (Coercible x s, Monoid s) => LActMn x (ActCoerce s)


-- | Semigroup action (monoid action when @Monoid s@)
instance {-# OVERLAPPABLE #-} (Semigroup s, Coercible x s)
  => RAct x (ActCoerce s) where
  x $<> ActCoerce s = coerce $ (coerce x :: s) <> s
  {-# INLINE ($<>) #-}

instance (Coercible x s, Semigroup s) => RActSg x (ActCoerce s)
instance (Coercible x s, Monoid s) => RActMn x (ActCoerce s)

-- | The trivial action where any element of @s@ acts as the identity function
-- on @x@
newtype ActId x = ActId  {unactId :: x}
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

-- | Action by morphism of monoids when @'Monoid' s@ and @'Monoid' x@
instance LAct x (ActId s) where
  (<>$) _ = id
  {-# INLINE (<>$) #-}

instance Semigroup s => LActSg x (ActId s)
instance Monoid s => LActMn x (ActId s)
instance Semigroup x => LActDistrib x (ActId s)
instance Monoid x => LActNeutral x (ActId s)

-- | Action by morphism of monoids when @'Monoid' s@ and @'Monoid' x@
instance RAct x (ActId s) where
  x $<> _ = x
  {-# INLINE ($<>) #-}

instance Semigroup s => RActSg x (ActId s)
instance Monoid s => RActMn x (ActId s)
instance Semigroup x => RActDistrib x (ActId s)
instance Monoid x => RActNeutral x (ActId s)

-- | An action on any functor that uses the @fmap@ function. For example :
--
-- @'ActMap' ('ActSelf' \"Hello\") <>$ [\" World\", " !"]  == ["Hello World", "Hello !"]@
newtype ActMap s = ActMap {unactMap :: s}
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

-- | Preserves the semigroup (resp. monoid) property of @'LAct' x s@, but
-- __not__ the morphism properties, which depend on potential @'Semigroup'@
-- (resp. @'Monoid'@) instances of @f x@
instance (LAct x s, Functor f) => LAct (f x) (ActMap s) where
  ActMap s <>$ x = fmap (s <>$) x
  {-# INLINE (<>$) #-}

instance (LActSg x s, Functor f) => LActSg (f x) (ActMap s)
instance (LActMn x s, Functor f) => LActMn (f x) (ActMap s)
instance LAct x s => LActDistrib [x] (ActMap s)
instance LAct x s => LActNeutral [x] (ActMap s)


-- | Preserves the semigroup (resp. monoid) property of @'LAct' x s@, but
-- __not__ the morphism properties, which depend on potential @'Semigroup'@
-- (resp. @'Monoid'@) instances of @f x@. When $f = []@, this is an action by morphism of monoids.
instance (RAct x s, Functor f) => RAct (f x) (ActMap s) where
  x $<> ActMap s = fmap ($<> s) x
  {-# INLINE ($<>) #-}

instance (RActSg x s, Functor f) => RActSg (f x) (ActMap s)
instance (RActMn x s, Functor f) => RActMn (f x) (ActMap s)
instance RAct x s => RActDistrib [x] (ActMap s)
instance RAct x s => RActNeutral [x] (ActMap s)

-- | An action @(<>$)@ can be feeded as an operator for the @'foldr'@ function,
-- allowing to lift any action to some @'Foldable'@ container. For example :
--
--  @'ActFold' ['Sum' (1 :: Int), 'Sum' 2, 'Sum' 3] <>$ (4 :: Int) == 10 :: Int @
newtype ActFold s = ActFold {unactFold :: s}
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid)

-- | When used with lists @[]@, this is a monoid action
instance (Foldable f, LAct x s) => LAct x (ActFold (f s)) where
  ActFold f <>$ x = foldr (<>$) x f
  {-# INLINE (<>$) #-}

instance LAct x s => LActSg x (ActFold [s])

-- | When used with lists @[]@, this is a monoid action
instance (Foldable f, RAct x s) => RAct x (ActFold (f s)) where
  x $<> ActFold f = foldl' ($<>) x f
  {-# INLINE ($<>) #-}


---------------------------------- Instances -----------------------------------

-- | Action by morphism of monoids
instance LAct x () where
  () <>$ x = x
  {-# INLINE (<>$) #-}

instance LActSg x ()
instance LActMn x ()
instance Semigroup x => LActDistrib x ()
instance Monoid x => LActNeutral x ()

-- | Monoid action
instance RAct x () where
  x $<> () = x
  {-# INLINE ($<>) #-}

instance RActSg x ()
instance RActMn x ()
instance Semigroup x => RActDistrib x ()
instance Monoid x => RActNeutral x ()

-- |  Action by morphism of semigroups (resp. monoids) when @'Semigroup' s@
-- (resp. @'Monoid' s@)
instance {-# INCOHERENT #-} LAct () s where
  _ <>$ () = ()
  {-# INLINE (<>$) #-}

instance {-# INCOHERENT #-} Semigroup s =>LActSg () s
instance {-# INCOHERENT #-} Monoid s =>  LActMn () s
instance {-# INCOHERENT #-} LActDistrib () s
instance {-# INCOHERENT #-} LActNeutral () s

-- |  Action by morphism of semigroups (resp. monoids) when @'Semigroup' s@
-- (resp. @'Monoid' s@)
instance {-# INCOHERENT #-} RAct () s where
  () $<> _ = ()
  {-# INLINE ($<>) #-}

instance {-# INCOHERENT #-} Semigroup s => RActSg () s
instance {-# INCOHERENT #-} Monoid s => RActMn () s
instance {-# INCOHERENT #-} RActDistrib () s
instance {-# INCOHERENT #-} RActNeutral () s

-- | Monoid action when @'LAct' x s@ is a semigroup action.
instance LAct x s => LAct x (Maybe s) where
  Nothing <>$ x = x
  Just s <>$ x = s <>$ x

instance LActSg x s => LActSg x (Maybe s)
instance LActSg x s => LActMn x (Maybe s)

-- | Monoid action when @'LAct' x s@ is a semigroup action.
instance RAct x s => RAct x (Maybe s) where
  x $<> Nothing = x
  x $<> Just s = x $<> s

instance RActSg x s => RActSg x (Maybe s)
instance RActSg x s => RActMn x (Maybe s)

-- | Same action propety as the weaker properties of @('LAct' x1 s1, 'LAct' x2
-- s2)@
instance (LAct x1 s1, LAct x2 s2) => LAct (x1, x2) (s1, s2) where
  (s1, s2) <>$ (x1, x2) = (s1 <>$ x1, s2 <>$ x2)

instance (LActSg x1 s1, LActSg x2 s2) => LActSg (x1, x2) (s1, s2)
instance (LActMn x1 s1, LActMn x2 s2) => LActMn (x1, x2) (s1, s2)
instance (LActDistrib x1 s1, LActDistrib x2 s2) => LActDistrib (x1, x2) (s1, s2)
instance (LActNeutral x1 s1, LActNeutral x2 s2) => LActNeutral (x1, x2) (s1, s2)

-- | Same action propety as the weaker properties of @('LAct' x1 s1, 'LAct' x2
-- s2)@
instance (RAct x1 s1, RAct x2 s2) => RAct (x1, x2) (s1, s2) where
  (x1, x2) $<> (s1, s2) = (x1 $<> s1, x2 $<> s2)

instance (RActSg x1 s1, RActSg x2 s2) => RActSg (x1, x2) (s1, s2)
instance (RActMn x1 s1, RActMn x2 s2) => RActMn (x1, x2) (s1, s2)
instance (RActDistrib x1 s1, RActDistrib x2 s2) => RActDistrib (x1, x2) (s1, s2)
instance (RActNeutral x1 s1, RActNeutral x2 s2) => RActNeutral (x1, x2) (s1, s2)

-- | No additionnal properties. In particular this is _not_ a semigroup action.
instance (LAct x s, LAct x t) => LAct x (Either s t) where
  (Left  s) <>$ x = s <>$ x
  (Right s) <>$ x = s <>$ x

-- | No additionnal properties. In particular this is _not_ a semigroup action.
instance (RAct x s, RAct x t) => RAct x (Either s t) where
  x $<> (Left  s) = x $<> s
  x $<> (Right s) = x $<> s


-------------------- Instances for base library functors ---------------------

-- | Preserves action properties of @'LAct' x s@.
instance LAct x s => LAct (Identity x) (Identity s) where
  Identity s <>$ Identity x = Identity (s <>$ x)

instance LActSg x s => LActSg (Identity x) (Identity s)
instance LActMn x s => LActMn (Identity x) (Identity s)
instance LActDistrib x s => LActDistrib (Identity x) (Identity s)
instance LActNeutral x s => LActNeutral (Identity x) (Identity s)

-- | Preserves action properties of @'LAct' x s@.
instance RAct x s => RAct (Identity x) (Identity s) where
  Identity x $<> Identity s = Identity (x $<> s)

instance RActSg x s => RActSg (Identity x) (Identity s)
instance RActMn x s => RActMn (Identity x) (Identity s)
instance RActDistrib x s => RActDistrib (Identity x) (Identity s)
instance RActNeutral x s => RActNeutral (Identity x) (Identity s)

------------------------- Instances for Data.Semigroup -------------------------

-- | Preserves action properties of @'LAct' x s@.
instance LAct x s => RAct x (Dual s) where
  x $<> Dual s = s <>$ x
  {-# INLINE ($<>) #-}

instance LActSg x s => RActSg x (Dual s)
instance LActMn x s => RActMn x (Dual s)
instance LActDistrib x s => RActDistrib x (Dual s)
instance LActNeutral x s => RActNeutral x (Dual s)

-- | Preserves action properties of @'LAct' x s@.
instance RAct x s => LAct x (Dual s) where
  Dual s <>$ x = x $<> s
  {-# INLINE (<>$) #-}

instance RActSg x s => LActSg x (Dual s)
instance RActMn x s => LActMn x (Dual s)
instance RActDistrib x s => LActDistrib x (Dual s)
instance RActNeutral x s => LActNeutral x (Dual s)

-- | Monoid action
instance LAct x (Endo x) where
  Endo f <>$ x = f x
  {-# INLINE (<>$) #-}

instance LActSg x (Endo x)
instance LActMn x (Endo x)

-- | Monoid action
instance Num x => LAct x (Sum x) where
  (<>$) s = coerce (s <>)
  {-# INLINE (<>$) #-}

instance Num x => LActSg x (Sum x)
instance Num x => LActMn x (Sum x)

-- | Monoid action
instance Num x => RAct x (Sum x) where
  x $<> s = coerce $ coerce x <> s
  {-# INLINE ($<>) #-}

instance Num x => RActSg x (Sum x)
instance Num x => RActMn x (Sum x)

-- | Monoid action
instance Num x => LAct x (Product x) where
  (<>$) s = coerce (s <>)
  {-# INLINE (<>$) #-}

instance Num x => LActSg x (Product x)
instance Num x => LActMn x (Product x)

-- | Monoid action
instance Num x => RAct x (Product x) where
  x $<> s = coerce $ coerce x <> s
  {-# INLINE ($<>) #-}

instance Num x => RActSg x (Product x)
instance Num x => RActMn x (Product x)

-- | Monoid action
instance {-# OVERLAPPING #-} Num x => LAct (Sum x) (Sum x) where
  (<>$) = (<>)
  {-# INLINE (<>$) #-}

instance {-# OVERLAPPING #-} Num x => LActSg (Sum x) (Sum x)
instance {-# OVERLAPPING #-} Num x => LActMn (Sum x) (Sum x)

-- | Monoid action
instance {-# OVERLAPPING #-} Num x => RAct (Sum x) (Sum x) where
  ($<>) = (<>)
  {-# INLINE ($<>) #-}

instance {-# OVERLAPPING #-} Num x => RActSg (Sum x) (Sum x)
instance {-# OVERLAPPING #-} Num x => RActMn (Sum x) (Sum x)

-- | Monoid action
instance {-# OVERLAPPING #-}  Num x => LAct (Product x) (Product x) where
  (<>$) s = coerce (s <>)
  {-# INLINE (<>$) #-}

instance {-# OVERLAPPING #-} Num x => LActSg (Product x) (Product x)
instance {-# OVERLAPPING #-} Num x => LActMn (Product x) (Product x)

-- | Monoid action
instance {-# OVERLAPPING #-} Num x => RAct (Product x) (Product x) where
  ($<>) = (<>)
  {-# INLINE ($<>) #-}

instance {-# OVERLAPPING #-} Num x => RActSg (Product x) (Product x)
instance {-# OVERLAPPING #-} Num x => RActMn (Product x) (Product x)

-- | Action by morphism of monoids
instance Num x => LAct (Sum x) (Product x) where
  (<>$) s = coerce (s <>)
  {-# INLINE (<>$) #-}

instance Num x => LActSg (Sum x) (Product x)
instance Num x => LActMn (Sum x) (Product x)
instance Num x => LActDistrib (Sum x) (Product x)
instance Num x => LActNeutral (Sum x) (Product x)

-- | Action by morphism of monoids
instance Num x => RAct (Sum x) (Product x) where
  x $<> s = coerce $ coerce x <> s
  {-# INLINE ($<>) #-}

instance Num x => RActSg (Sum x) (Product x)
instance Num x => RActMn (Sum x) (Product x)
instance Num x => RActDistrib (Sum x) (Product x)
instance Num x => RActNeutral (Sum x) (Product x)

-- | Monoid action
instance LAct Bool Any where
  (<>$) s = coerce (s <>)
  {-# INLINE (<>$) #-}

instance LActSg Bool Any
instance LActMn Bool Any

-- | Monoid action
instance RAct Bool Any where
  x $<> s = coerce $ coerce x <> s
  {-# INLINE ($<>) #-}

instance RActSg Bool Any
instance RActMn Bool Any

-- | Monoid action
instance LAct Bool All where
  (<>$) s = coerce (s <>)
  {-# INLINE (<>$) #-}

instance LActSg Bool All
instance LActMn Bool All

-- | Monoid action
instance RAct Bool All where
  x $<> s = coerce $ coerce x <> s
  {-# INLINE ($<>) #-}

instance RActSg Bool All
instance RActMn Bool All

-- | Semigroup action
instance LAct x (S.First x) where
  (<>$) s = coerce (s <>)
  {-# INLINE (<>$) #-}

instance LActSg x (S.First x)

-- | Semigroup action
instance RAct x (S.Last x) where
  x $<> s = coerce $ coerce x <> s
  {-# INLINE ($<>) #-}

instance RActSg x (S.Last x)

-- | Monoid action
instance LAct x (M.First x) where
  M.First Nothing <>$ x = x
  M.First (Just s) <>$ _ = s
  {-# INLINE (<>$) #-}

instance LActSg x (M.First x)
instance LActMn x (M.First x)

-- | Monoid action
instance RAct x (M.Last x) where
  x $<> M.Last Nothing = x
  _ $<> M.Last (Just s) = s
  {-# INLINE ($<>) #-}

instance RActSg x (M.Last x)
instance RActMn x (M.Last x)


---------------------------- Instances for Linear ----------------------------

-- | Monoid action
instance Semigroup a => LAct (V1 a) (V1 a) where
  (<>$) = (<>)
  {-# INLINE (<>$) #-}

instance Semigroup a => LActSg (V1 a) (V1 a)
instance Monoid a => LActMn (V1 a) (V1 a)

-- | Monoid action
instance Semigroup a => RAct (V1 a) (V1 a) where
  ($<>) = (<>)
  {-# INLINE ($<>) #-}

instance Semigroup a => RActSg (V1 a) (V1 a)
instance Monoid a => RActMn (V1 a) (V1 a)

-- | Monoid action
instance Semigroup a => LAct (V2 a) (V2 a) where
  (<>$) = (<>)
  {-# INLINE (<>$) #-}

instance Semigroup a => LActSg (V2 a) (V2 a)
instance Monoid a => LActMn (V2 a) (V2 a)

-- | Monoid action
instance Semigroup a => RAct (V2 a) (V2 a) where
  ($<>) = (<>)
  {-# INLINE ($<>) #-}

instance Semigroup a => RActSg (V2 a) (V2 a)
instance Monoid a => RActMn (V2 a) (V2 a)

-- | Monoid action
instance Semigroup a => LAct (V3 a) (V3 a) where
  (<>$) = (<>)
  {-# INLINE (<>$) #-}

instance Semigroup a => LActSg (V3 a) (V3 a)
instance Monoid a => LActMn (V3 a) (V3 a)

-- | Monoid action
instance Semigroup a => RAct (V3 a) (V3 a) where
  ($<>) = (<>)
  {-# INLINE ($<>) #-}

instance Semigroup a => RActSg (V3 a) (V3 a)
instance Monoid a => RActMn (V3 a) (V3 a)

-- | Monoid action
instance Semigroup a => LAct (V4 a) (V4 a) where
  (<>$) = (<>)
  {-# INLINE (<>$) #-}

instance Semigroup a => LActSg (V4 a) (V4 a)
instance Monoid a => LActMn (V4 a) (V4 a)

-- | Monoid action
instance Semigroup a => RAct (V4 a) (V4 a) where
  ($<>) = (<>)
  {-# INLINE ($<>) #-}

instance Semigroup a => RActSg (V4 a) (V4 a)
instance Monoid a => RActMn (V4 a) (V4 a)