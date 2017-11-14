 {-# LANGUAGE TemplateHaskell #-}
 
import Task01
import Test.QuickCheck
import Test.Hspec

prop_getMinPath_preserves p =
  getOffset (getMinPath p) == p

inputGen = sized $ \n -> vectorOf n $ elements "NEWS"

prop_solve_shorter =
  forAll inputGen $ \s -> length (solve s) <= length s

test_getOffset =
  describe "getOffset" $ do
    it "works on empty string" $ do
      (getOffset "") `shouldBe` (0, 0)
    it "works on some examples" $ do
      (getOffset "NEWS") `shouldBe` (0, 0)
      (getOffset "NESEN") `shouldBe` (2, 1)

return []

main = do
  hspec $ test_getOffset
  $quickCheckAll
