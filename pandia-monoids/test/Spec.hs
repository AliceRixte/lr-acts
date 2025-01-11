{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE OverloadedLabels #-}

import Test.Hspec
import Test.QuickCheck

import Data.Monoid
import Data.Act

main :: IO ()
main = hspec $ do
  describe "Action" $ do
    describe "ActSelf" $ do
      it "Int acts on unit" $ property $
        \x -> (x :: Int) <>$ () `shouldBe` ()
      it "Unit acts on char" $ property $
        \x -> () <>$ (x :: Char) `shouldBe` x

-- haha = Sum (1::Int) <>$ [Sum (2::Int)]
-- y0 = [ActSelf [Sum (1::Int)], ActSelf [Sum (1::Int)]] <>$  [Sum (2::Int)]
-- y2 = ActSelf [[Sum (1::Int)], [Sum (1::Int)]] <>$  [[Sum (2::Int)]]
-- y3 =  [[Sum (1::Int)], [Sum (1::Int)]] <>$ [[Sum (2::Int)]]
-- y1 = [[Sum (1::Int)]] <>$  [Sum (2::Int)]

-- y5 = ActMap (ActSelf (Sum (1 :: Int))) <>$ (Sum 2 :: Sum Int, Sum 3 :: Sum Int)
