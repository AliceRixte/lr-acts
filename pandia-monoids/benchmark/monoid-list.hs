{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds#-}
{-# LANGUAGE TypeOperators#-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses#-}
{-# LANGUAGE TypeApplications#-}

module Main (main) where

import Criterion.Main

import Data.Semigroup
import Data.Extensible hiding (embed)

import Data.Monoid.Action
import Data.Monoid.MList

import Data.Act

import Control.Field
import qualified Data.PRecord.Internal.PRecord as Rec1
import qualified Data.PRecord.Internal.PRecord2 as Rec2

-- import Data.Extensible.Internal.Field.Orphan ()

type CoprodMList =
   Product Int
  -- ::: Product Int
  -- ::: Product Int
  -- ::: Product Int
  -- ::: Product Int
  ::: Product Int
  ::: Product Int
  ::: Product Int
  ::: Product Int
  ::: Product Int
  ::: Product Int
  ::: Product Int
  ::: Sum Int
  ::: ()


type CoprodList =  '[
    "Bla" :> Product Int
  , "Blaa" :> Product Int
  , "Blab" :> Product Int
  , "Blac" :> Product Int
  , "Blad" :> Product Int
  , "Blae" :> Product Int
  , "Blaf" :> Product Int
  , "Blag" :> Product Int
  , "Blah" :> Product Int
  , "Blai" :> Product Int
  , "Blaj" :> Product Int
  , "Bla" :> Product Int
  , "Blaa" :> Product Int
  , "Blab" :> Product Int
  , "Blac" :> Product Int
  , "Blad" :> Product Int
  , "Blae" :> Product Int
  , "Blaf" :> Product Int
  , "Blag" :> Product Int
  , "Blah" :> Product Int
  , "Blai" :> Product Int
  , "Blaj" :> Product Int
  , "Blak" :> Product Int
  , "Blal" :> Product Int
  , "Time" :> Sum Int -- long list to see performance overhead
  ]

type CoprodBench1 = Rec1.PRecord CoprodList
type CoprodBench2 = Rec2.PRecord CoprodList


------------------------------------ (<>) ------------------------------------
copList :: Int -> CoprodMList
copList 0 =  empty
copList n =  (inj (Sum n) :: CoprodMList)  <> copList (n-1)

cop1 :: Int -> CoprodBench1
cop1 0 =  embed @"Time" (Sum 0)
cop1 n =  embed @"Time" (Sum n) <> cop1 (n-1)

-- cop2 :: Int -> CoprodBench2
-- cop2 0 =  embed @"Time" (Sum 0)
-- cop2 n =  embed @"Time" (Sum n) <> cop2 (n-1)


instance Action (Sum Int) (Sum Int) where
  act = (+)
instance Action (Product Int) (Product Int) where
  act = (*)
instance Action (Sum Int) (Product Int) where
  act (Sum n) (Product m)=  Product (n + m)
instance Action (Product Int) (Sum Int) where
  act (Product n) (Sum m)=  Sum (n * m)

actList :: Int -> CoprodMList
actList 0 =  (inj (Sum (0 :: Int)) :: CoprodMList)
actList n =  act (inj (Sum n) :: CoprodMList) (actList (n-1))

-- act1 :: Int -> CoprodBench1
-- act1 0 =  embed @"Time" (Sum 0)
-- act1 n =  (embed  @"Time" (Sum n) :: CoprodBench1) <>$ act1 (n-1)

-- act2 :: Int -> CoprodBench2
-- act2 0 =  embed @"Time" (Sum 0)
-- act2 n =  (embed  @"Time" (Sum n) :: CoprodBench2) <>$ act2 (n-1)



-- pairs :: Int -> (Sum Int, Sum Int)
-- pairs 0 = (Sum 0, Sum 0)
-- pairs n = (Sum n, Sum n)  <> pairs (n - 1)

type Tuple6 a = (a,a,a)
pairs :: Int -> Tuple6 (Maybe (Sum Int))
pairs 0 = (Just (Sum (0 :: Int)), Just (Sum (0 :: Int)),Just (Sum (0 :: Int)))
pairs n = (Just $Sum n,  Just (Sum (0 :: Int)), Just (Sum (0 :: Int)))  <> pairs (n - 1)

sumS :: Int -> Sum Int
sumS 0 = Sum 0
sumS n = Sum n <> sumS (n - 1)

---------------------------- injection extraction ----------------------------

getMList :: Int -> Maybe (Sum Int)
getMList 0 = Just 0
getMList n = (get (inj (Sum n) :: CoprodMList) :: Maybe (Sum Int)) <>  getMList (n - 1)

extract1 :: Int -> Sum Int
extract1 0 = Sum 0
extract1 n = (Rec1.extract #Time (embed @"Time" (Sum n) :: CoprodBench1))
                <>  extract1 (n - 1)

-- extract2 :: Int -> Sum Int
-- extract2 0 = Sum 0
-- extract2 n = (Rec2.extract #Time (embed @"Time" (Sum n) :: CoprodBench2))
--                 <>  extract2 (n - 1)


mkBench f n = bench (show n) $ nf f n

iterations :: [Int]
iterations = [10, 100, 1000]

main :: IO()
main = do
  print (actList 5)
  defaultMain [
    bgroup "(<>$)" [
       bgroup "MList" (fmap (mkBench $
            (\l -> get l :: Maybe (Sum Int)) . actList)
                              iterations)
      -- , bgroup "Coprod1" (fmap (mkBench $ Rec1.extract #Time . act1)
      --                         iterations)
      -- , bgroup "Coprod2" (fmap (mkBench $ Rec2.extract #Time . act2))
      , bgroup "Pairs " (fmap (mkBench pairs) iterations)
      , bgroup "Sum"    (fmap (mkBench sumS) iterations)
    ]
    , bgroup "Inject/extract" [
        bgroup "MList" (fmap (mkBench $ getMList)
                              iterations)

        , bgroup "Coprod1" (fmap (mkBench $ extract1) iterations)
        -- , bgroup "Coprod2" (fmap (mkBench $ extract2) iterations)
    ]
    , bgroup "(<>)" [
        bgroup "Sum"    (fmap (mkBench sumS) iterations)
      , bgroup "MList" (fmap (mkBench $
            (\l -> get l :: Maybe (Sum Int)) . copList)
                              iterations)
      , bgroup "Coprod1" (fmap (mkBench $ Rec1.extract #Time . cop1)
                              iterations)
      -- , bgroup "Coprod2" (fmap (mkBench $ Rec2.extract #Time . cop2)iterations)

      , bgroup "Pairs " (fmap (mkBench pairs) iterations)
      , bgroup "Sum"    (fmap (mkBench sumS) iterations)
    ]
    ]