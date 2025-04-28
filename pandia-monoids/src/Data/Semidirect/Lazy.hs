{-# LANGUAGE FlexibleInstances            #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE InstanceSigs                 #-}
{-# LANGUAGE ScopedTypeVariables          #-}

-----------------------------------------------------------------------------
-- |
--   Module      :  Data.Semigroup.Semidirect.Lazy
--   Copyright   :  (c) Alice Rixte (2024)
--   License     :  BSD 3 (see LICENSE)
--   Maintainer  :  alice.rixte@u-bordeaux.fr
--
-- Semidirect products
-----------------------------------------------------------------------------

module Data.Semigroup.Semidirect.Lazy
       ( LSemidirect (..)
       , lerase
       , lforget
       , lfromActee
       , lfromActor
       , lfromPair
        , RSemidirect (..)
        , rerase
        , rforget
        , rfromActee
        , rfromActor
        , rfromPair
       ) where

import Data.Bifunctor
import Data.Act

-- | A semi-direct product for a left action, where @s@ acts on @x@
--
data LSemidirect x s = LSemidirect
  { lactee :: x -- ^ The value being acted on
  , lactor :: s -- ^ The acting element
  }
  deriving (Show, Read, Eq)

instance LActSgMorph x s
  => Semigroup (LSemidirect x s) where
  ~(LSemidirect x s) <> ~(LSemidirect x' s') =
    LSemidirect  (x <> (s <>$ x')) (s <> s')

instance LActMnMorph x s => Monoid (LSemidirect x s) where
  mempty = LSemidirect mempty mempty

instance Functor (LSemidirect x) where
  fmap f a = a {lactor = f (lactor a)}

instance Bifunctor LSemidirect where
  first f a = a {lactee = f (lactee a)}
  second = fmap

-- |  Erases the element being acted on (i.e. replace it with @mempty@).
lerase :: Monoid x => LSemidirect x s -> LSemidirect x s
lerase a = a {lactee = mempty}

-- |  Forget the acting element (i.e. replace it with @mempty@).
lforget :: Monoid s => LSemidirect x s -> LSemidirect x s
lforget a =a {lactor = mempty}

-- |  Make a semidirect pair out whose actee element is @mempty@.
lfromActor :: Monoid x => s -> LSemidirect x s
lfromActor s = LSemidirect mempty s

-- |  Make a semidirect pair out whose actor element is @mempty@ .
lfromActee :: Monoid s => x -> LSemidirect x s
lfromActee x = LSemidirect x mempty

-- | Converts a pair into a semidirect product element
lfromPair :: (x,s) -> LSemidirect x s
lfromPair (x,s) = LSemidirect x s


------------------------------------------------------------------------------

-- |  A semidirect product for a right action, where @s@ acts on @x@
--
data RSemidirect x s = RSemidirect
  { ractee :: x -- ^ The value being acted on
  , ractor :: s -- ^ The acting element
  }
  deriving (Show, Read, Eq)

instance RActSgMorph x s
  => Semigroup (RSemidirect x s) where
  ~(RSemidirect x s) <> ~(RSemidirect x' s') =
    RSemidirect  (x <> (x' $<> s)) (s <> s')

instance RActMnMorph x s => Monoid (RSemidirect x s) where
  mempty = RSemidirect mempty mempty

instance Functor (RSemidirect x) where
  fmap f a = a {ractor = f (ractor a)}

instance Bifunctor RSemidirect where
  first f a = a {ractee = f (ractee a)}
  second = fmap

-- |  Erases the element being acted on (i.e. replace it with @mempty@).
rerase :: Monoid x => RSemidirect x s -> RSemidirect x s
rerase a = a {ractee = mempty}

-- |  Forget the acting element (i.e. replace it with @mempty@).
rforget :: Monoid s => RSemidirect x s -> RSemidirect x s
rforget a = a {ractor = mempty}

-- |  Make a semidirect pair out whose actee element is @mempty@.
rfromActor :: Monoid x => s -> RSemidirect x s
rfromActor s = RSemidirect mempty s

-- |  Make a semidirect pair out whose actor element is @mempty@ .
rfromActee :: Monoid s => x -> RSemidirect x s
rfromActee x = RSemidirect x mempty

-- | Converts a pair into a semidirect product element
rfromPair :: (x,s) -> RSemidirect x s
rfromPair (x,s) = RSemidirect x s
