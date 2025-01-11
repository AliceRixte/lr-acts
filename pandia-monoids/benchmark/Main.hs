module Main (main) where

import Criterion.Main

import Data.Semigroup.Semidirect.Lazy as L
-- import Data.Semigroup.Semidirect.Strict as S
import Data.Act

import Data.Monoid
import Data.Semigroup

-- stimesSemiL :: Int -> L.LSemidirect (Sum Int) (Product Int)
-- stimesSemiL n = stimes n (L.lfromPair (Sum (1::Int), Product (1::Int)))

-- stimesSemiS :: Int -> S.LSemidirect (Sum Int) (Cayley (Sum Int))
-- stimesSemiS n = stimes n (S.lfromPair (Sum (1::Int), Cayley (Sum (1::Int))))

-- The strict version is twice slower than the lazy one. Why ?
---------------------------------------  ---------------------------------------
blub :: Int  -> ()
blub n = let s = sum [1..n] in
  ()

blab :: Int  -> (Int, ())
blab n = let s = sum [1..n] in
  (s, ())

blob :: Int -> ()
blob = snd . blab

blib :: Int -> ()
blib n = [1..n] <>$ ()

main :: IO ()
main =
    defaultMain [
    --   bgroup "Sum" [
    --     bench "100" $   nf blib 100
    --   , bench "1000" $  nf blib 1000
    --   , bench "10000" $ nf blib 10000
    --   ]
    --  , bgroup "Sum" [
    --     bench "100" $   nf blab 100
    --   , bench "1000" $  nf blab 1000
    --   , bench "10000" $ nf blab 10000
    --   ]
    -- , bgroup "Semidirect" [
    --     bgroup "Lazy" [
    --       bench "100"   $ whnf stimesSemiL 100
    --     , bench "1000"  $ whnf stimesSemiL 1000
    --     , bench "10000" $ whnf stimesSemiL 10000
    --     ]
      -- , bgroup "Strict" [
      --     bench "100"   $ whnf stimesSemiS 100
      --   , bench "1000"  $ whnf stimesSemiS 1000
      --   , bench "10000" $ whnf stimesSemiS 10000
      --   ]

    ]