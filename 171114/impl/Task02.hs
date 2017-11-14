{-# LANGUAGE TemplateHaskell #-}

import Chr
import Test.QuickCheck
import Test.Hspec
import Data.List
import Data.Maybe

coin1 = Func "coin100" []
coin2 = Func "coin200" []
coin50 = Func "coin50" []
coin20 = Func "coin20" []
coin10 = Func "coin10" []

prices = [100, 200, 50, 20, 10]::[Int]
getCost (Func ('c':'o':'i':'n':s) []) = (read s)::Int
sumCost xs = sum $ map getCost xs

coinOf p = Func ("coin" ++ show p) []
genCoin = fmap coinOf (elements prices)
genCoins = sized $ \n -> vectorOf n genCoin
genCoinsWithSum 0 = elements [[]]
genCoinsWithSum x =
  do
    y <- elements $ filter (<= x) prices
    res <- genCoinsWithSum (x - y)
    return $ coinOf y : res

rules =
  [
    [coin1, coin1] <=> [coin2],
    [coin50, coin50] <=> [coin1],
    [coin20, coin20, coin20, coin20, coin20] <=> [coin1],
--    [coin20, coin20, coin20] <=> [coin50, coin10],
    [coin20, coin20, coin10] <=> [coin50],
    [coin10, coin10] <=> [coin20]
  ]

prop_preserve_sum =
  forAll genCoins $ \x -> sumCost x == sumCost (fromJust $ eval rules x)

prop_shorter =
  forAll genCoins $ \x -> length x >= length (fromJust $ eval rules x)

prop_shortest (NonNegative s) =
  forAll (genCoinsWithSum (s * 10)) $ \x ->
  forAll (genCoinsWithSum (s * 10)) $ \y ->
    length x >= length (fromJust $ eval rules y)

a `shouldBeNoOrd` b = (fmap sort a) `shouldBe` (fmap sort b)

return []

main = do
  hspec $ describe "Task01" $ do
    it "works on empty test" $ do
      (eval rules []) `shouldBeNoOrd` (Just [])
    it "works on some examples" $ do
--      (eval rules [coin20, coin20, coin20]) `shouldBeNoOrd` (Just [coin50, coin10])
      (eval rules (take 39 $ repeat coin10)) `shouldBeNoOrd` (Just [coin1, coin2, coin50, coin20, coin20])
  $forAllProperties (quickCheckWithResult stdArgs { maxSuccess = 300 })
