{-# LANGUAGE PackageImports #-}

-----------------------------------------------------------------------------
-- |
-- Module      : Data.Monoid.Coproduct
-- Description : Coproducts of monoids.
-- Copyright   : (c) Alice Rixte 2025
-- License     : BSD 3
-- Maintainer  : alice.rixte@u-bordeaux.fr
-- Stability   : unstable
-- Portability : non-portable (GHC extensions)
--
-- Coproduct of two monoids.
--
---------------------------------------------------------------------------

module Data.Monoid.Coproduct
  ( (:+:)
  , inL, inR
  , mappendL, mappendR
  , cop
  , killL, killR
  , toAltList
  , toReducedAltList
  , untangle
  , luntangleSemi

  ) where

import Data.Semidirect.Lazy
import Data.Act
import "monoid-extras" Data.Monoid.Coproduct as Extra
import "monoid-extras" Data.Monoid.SemiDirectProduct as Extra


-- | The copairing of @embed@ and @inject@ homomorphisms into the
--   semidirect product. Note that @embed@ and @inject@ are monoid
--   homomorphisms. Therefore @untangleSemi@ is also a monoid homomorphism.
luntangleSemi :: (LActMnMorph n m, Monoid m, Monoid n)
  => m :+: n -> LSemidirect n m
luntangleSemi = lfromActor `cop` lfromActee

-- | The copairing of @embed@ and @inject@ homomorphisms into the
--   semidirect product. Note that @embed@ and @inject@ are monoid
--   homomorphisms. Therefore @untangleSemi@ is also a monoid homomorphism.
luntangleSemi :: (LActMnMorph n m, Monoid m, Monoid n)
  => m :+: n -> LSemidirect n m
luntangleSemi = lfromActor `cop` lfromActee



-- | Same as @untangleSemi@ but the result is uwrapped. Concretely, given
--   a value from a coproduct monoid where the left monoid has an
--   action on the right, and \"untangle\" it into a pair of values.  In
--   particular,
--
-- > m1 <> n1 <> m2 <> n2 <> m3 <> n3 <> ...
--
--   is sent to
--
-- > (m1 <> m2 <> m3 <> ..., (act m1 n1) <> (act (m1 <> m2) n2) <> (act (m1 <> m2 <> m3) n3) <> ...)
--
--   That is, before combining @n@ values, every @n@ value is acted on
--   by all the @m@ values to its left.
-- untangle :: (Action m n, Monoid m, Monoid n) => m :+: n -> (m,n)
-- untangle = swap . unSemi . untangleSemi