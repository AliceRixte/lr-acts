{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeOperators    #-}
{-# OPTIONS_GHC -ddump-to-file #-}

module Main (main) where

import Criterion.Main

import Data.Semidirect.Lazy as L
import Data.Semidirect.Strict as S

import Data.Monoid
import Data.Semigroup
import Data.Act

import GHC.Real

--------------------------------- Semidirect ---------------------------------

stimesLSemiLazy :: Int -> Sum Int
stimesLSemiLazy n =   L.lactee $ stimes n
    (L.LSemidirect (Sum 1) (Product 2) :: L.LSemidirect (Sum Int) (Product Int))

stimesLSemiStrict :: Int -> Sum Int
stimesLSemiStrict n =
  S.lactee $ stimes n
    (S.LSemidirect (Sum 1) (Product 2) :: S.LSemidirect (Sum Int) (Product Int))

sumProduct :: Int  -> (Sum Int, Product Int)
sumProduct n = stimes n (Sum 1, Product 2)

--------------------------------- LDefault  ----------------------------------

mulDef :: Int -> Double
mulDef 0 = 0
mulDef n = lorigin @(LDefault (2 :% 3) Double) + mulDef (n-1)

mulDouble :: Int -> Double
mulDouble 0 = 0
mulDouble n = 2/3 + mulDouble (n-1)



------------------------------------------------------------------------------

mkBench f n = bench (show n) $ nf f n

pow10list :: Int -> Int -> [Int]
pow10list a b = [10 ^n | n <- [a..b]]

nlist  :: [Int]
nlist = pow10list 1 4


main :: IO ()
main =
    defaultMain
      [  bgroup "ActCyclic" [
          bgroup "Double "            (fmap (mkBench mulDouble)      nlist)
        , bgroup "LDefault Ratio"     (fmap (mkBench mulDef)      nlist)
        ]

      , bgroup "Semidirect" [
          bgroup "Lazy pair (,)"      (fmap (mkBench sumProduct)      nlist)
        , bgroup "Lazy LSemidirect"   (fmap (mkBench stimesLSemiLazy) nlist)
        , bgroup "Strict LSemidirect" (fmap (mkBench stimesLSemiStrict) nlist)
        ]
      ]