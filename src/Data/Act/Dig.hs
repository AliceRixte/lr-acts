{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

--------------------------------------------------------------------------------
-- |
--
-- Module      :  Data.Act.Dig
-- Description :  Actions of sets, semigroups, monoids and groups.
-- Copyright   :  (c) Alice Rixte 2024
-- License     :  BSD 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  unstable
-- Portability :  non-portable (GHC extensions)
--
-- = Usage
--
-- For both @'LAct'@ and @'RAct'@, the acting type is the second parameter. This
-- is a bit counter intuitive when using @'LAct'@, but it allows to use the
-- @DerivingVia@ mechanism to derive instances of @'LAct'@ and @'RAct'@ for
-- newtypes that wrap the acting type. For example, you can use @'ActSelf''@ as
-- follow to derive instances for @'LAct'@ and @'RAct'@ :
--
-- @
-- {-# LANGUAGE DerivingVia #-}
--
-- import Data.Act
-- import Data.Semigroup
--
-- newtype Seconds = Seconds Float
-- newtype Duration = Duration Seconds
--   deriving (Semigroup, Monoid) via (Sum Float)
--
--   deriving ('LAct' Seconds, 'RAct' Seconds) via ('ActSelf'' (Sum Float))
--   -- derives LAct Second  Duration
--
--   deriving ('LAct' [Seconds], RAct [Seconds]) via ('ActMap' ('ActSelf'' (Sum Float)))
--    -- derives LAct [Second] Duration
--
-- newtype Durations = Durations [Duration]
--   deriving ('LAct' Seconds, 'RAct' Seconds) via ('ActFold' [Duration])
--   -- derives LAct Second Durations
-- @
-- >>> Duration (Seconds 1) <>$ (Seconds 2)
-- Seconds 3.0
-- >>> Duration 2 <>$ Seconds 3
-- Seconds 5.0
-- >>> Duration 2 <>$ [Seconds 3, Seconds 4]
-- [Seconds 5.0,Seconds 6.0]
-- >>> [Duration 2, Duration 3] <>$ Seconds 4
-- [Seconds 5.0,Seconds 6.0]
-- >>> Durations [Duration 2, Duration 3] <>$ Seconds 4
-- Seconds 9.0
--
--
--------------------------------------------------------------------------------

module Data.Act.Dig
  ( Dig (..)
  , ActDig (..)
  , ActDeep (..)
  , pattern ActDeep
  , (+$)
  , (+$$)
  , (+$$$)
  , (+$&)
) where

import GHC.TypeLits

import Data.Semigroup as Sg
import Data.Monoid
import Data.Group
import Data.Functor.Identity
import Data.Foldable
import Data.Coerce


import Witch
import Witch.Orphans ()

import Data.Act.Act

(+$) :: LAct x (ActShallow (Sum s)) => s -> x -> x
s +$ x = ActShallow (Sum s) <>$ x

-- ff :: LAct Int (Sum a) => a -> [Int]
ff c = c +$ [1]
ff' c = c +$ (1 :: Double, 2 :: Float) --[1 :: Int]

data Dig = Once | Indefinitely

type family Shallower' d = r | r -> d  where
  Shallower' (Just Once)  = Nothing
  Shallower' (Just Indefinitely)  = Just Indefinitely

type Shallower d = Shallower' (Just d)

-- | Acts on every element of a structure.
--
-- Typically :
--
-- * it acts on sum types by acting on their content
-- * it acts on product types by acting on each component and then combining the
--   results using the semigroup operation of @s@
--
-- Contrarily to @'ActDeep@, this action will only try to deconstruct @x@ once.
--
-- >>> ActShallow (Sum 1) <>$ [1, 2, 3 ]
-- [2,3,4]
--
-- >>> ActShallow (Sum (1 :: Int)) <>$ (1 :: Int, 2.5 :: Float, pi :: Double)
-- (2,3.5,4.141592653589793)
--
newtype ActDig' (d :: Maybe Dig) s = ActDig' {unactDig :: s}
  deriving stock (Show, Eq)
  deriving newtype (Semigroup, Monoid, Group)

type ActDig d s = ActDig' (Just d) s
type ActShallower d s = ActDig' (Shallower d) s
type ActShallow s = ActDig Once s
type ActDeep s = ActDig Indefinitely s

(+$&) :: LAct x (ActDeep (Sum s)) => s -> x -> x
(+$&) s = (ActDeep (Sum s) <>$)

(+$$) :: LAct x (ActShallow (Sum s)) => s -> x -> x
(+$$) s = (ActShallow (Sum s) <>$)

(+$$$) :: LAct x (ActShallow (ActShallow (Sum s))) => s -> x -> x
(+$$$) s = (ActShallow (ActShallow (Sum s)) <>$)

pattern ActDig :: forall d s. s -> ActDig d s
pattern ActDig s = ActDig' @(Just d) s

pattern ActShallow :: s -> ActShallow s
pattern ActShallow s = ActDig' @(Just Once) s

pattern ActShallower :: forall d s. s -> ActShallower (d :: Dig) s
pattern ActShallower s = ActDig' @(Shallower d) s

pattern ActDeep :: s -> ActDeep s
pattern ActDeep s = ActDig @Indefinitely s


instance LAct x s => LAct x (ActDig' 'Nothing s) where
  ActDig' s <>$ x = s <>$ x
  {-# INLINE (<>$) #-}

instance {-# INCOHERENT #-}
  LAct x s
  => LAct x (ActDig d s) where
  ActDig s <>$ x =  s <>$ x
  {-# INLINE (<>$) #-}

instance
  ( LAct x0 (ActShallower d s)
  , LAct x1 (ActShallower d s)
  )
  => LAct (Either x0 x1) (ActDig d s) where
  ActDig s <>$ Left  x0 = Left  $ ActShallower @d s <>$ x0
  ActDig s <>$ Right x1 = Right $ ActShallower @d s <>$ x1
  {-# INLINE (<>$) #-}

instance
  ( LAct x (ActShallower d s))
  => LAct [x] (ActDig d s) where
  ActDig s <>$ x = map (ActShallower @d s <>$) x
  {-# INLINE (<>$) #-}

instance
  ( LAct x0 (ActShallower d s)
  , LAct x1 (ActShallower d s)
  )
  => LAct (x0, x1) (ActDig d s) where
  ActDig s <>$ (x0, x1) = (ActShallower @d s <>$ x0, ActShallower @d s <>$ x1)
  {-# INLINE (<>$) #-}

instance
  ( LAct x0 (ActShallower d s)
  , LAct x1 (ActShallower d s)
  , LAct x2 (ActShallower d s)
  )
  => LAct (x0, x1, x2) (ActDig d s) where
  ActDig s <>$ (x0, x1, x2) = (ActShallower @d s <>$ x0, ActShallower @d s <>$ x1, ActShallower @d s <>$ x2)
  {-# INLINE (<>$) #-}
