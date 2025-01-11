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
       , lactor
       , lactee
       , lerase
       , lreset
       , lfromActee
       , lfromActor
       , lfromPair
       ) where

import Data.Bifunctor
import Data.Act

import Data.Coerce

-- import Data.Aeson

-- | A semi-direct product for a left action, where @s@ acts on @x@
--
newtype LSemidirect x s = LSemidirect {lunsemidirect :: (x,s)}
  deriving (Show, Read, Eq) --, ToJSON, FromJSON)

instance (Semigroup s, Semigroup x, LAct x s)
  => Semigroup (LSemidirect x s) where
  ~(LSemidirect (n1,h1)) <> ~(LSemidirect (n2,h2)) =
    LSemidirect  (n1 <> (h1 <>$ n2), h1 <> h2)


instance (Monoid s, Monoid x, LAct x s) => Monoid (LSemidirect x s) where
  mempty = LSemidirect (mempty :: x , mempty :: s)


instance Functor (LSemidirect x) where
  fmap :: forall a b. (a -> b) -> LSemidirect x a -> LSemidirect x b
  fmap = coerce (fmap :: (a -> b) -> (x,a) -> (x,b))

instance Bifunctor LSemidirect where
  bimap :: forall a b c d.
    (a -> b) -> (c -> d) -> LSemidirect a c -> LSemidirect b d
  bimap = coerce (bimap :: (a -> b) -> (c -> d) -> (a,c) -> (b,d))

-- | Get the value being acted on of a semidirect pair
lactee :: forall x s. LSemidirect x s -> x
lactee = coerce (fst :: (x,s) -> x)

-- | Get the acting element of a semidirect pair
lactor :: forall x s. LSemidirect x s -> s
lactor = coerce (snd :: (x,s) -> s)

-- |  Erases the element being acted on.
lerase :: Monoid x => LSemidirect x s -> LSemidirect x s
lerase (LSemidirect (_,s)) = LSemidirect (mempty,s)


-- |  Resets the acting element.
--
--  See the article "Unified media programming: An algebraic approach" by
--  S.Archipoff and D. Janin to understand why this function is called reset.
--
lreset :: Monoid s => LSemidirect x s -> LSemidirect x s
lreset (LSemidirect (x,_)) = LSemidirect (x,mempty)


-- |  Creates a pair out of an acting element
lfromActor :: Monoid x => s -> LSemidirect x s
lfromActor s = LSemidirect (mempty,s)

-- |  Creates a pair out of an element being acted on
lfromActee :: Monoid s => x -> LSemidirect x s
lfromActee x = LSemidirect (x,mempty)


-- | Converts a pair into a semidirect product element
lfromPair :: (x,s) -> LSemidirect x s
lfromPair = coerce
