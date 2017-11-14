{-# LANGUAGE TemplateHaskell #-}

import Task02
import Test.QuickCheck
import Test.Hspec

instance Arbitrary Coin where
  arbitrary = arbitraryBoundedEnum

prop_getOptimalSplit_preserves (NonNegative x) =
  let value = x * 10 in
  getSum (getOptimalSplit value) == value

prop_solve_shorter s =
  length (solve s) <= length s

test_getSum =
  describe "getSum" $ do
    it "works on empty list" $ do
      (getSum []) `shouldBe` 0
    it "works on some examples" $ do
      (getSum [E2, C10, C50, E1, C10, C20]) `shouldBe` 390

return []

main = do
  hspec $ test_getSum
  $forAllProperties (quickCheckWithResult stdArgs { maxSuccess = 10000 })
