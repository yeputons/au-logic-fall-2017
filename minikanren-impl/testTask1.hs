import Test.Hspec
import Data.Maybe
import Minikanren
import Peano
import Lists
import Task1

x = Var "x"
y = Var "y"

shouldBeXy real expected = map (\(PSol e d) -> (fromJust $ lookup "x" e, fromJust $ lookup "y" e, d)) real `shouldBe` expected

main :: IO ()
main = hspec $ do
  describe "incrementDigito" $ do
    it "works on (0, 0)" $ do
      (solve $ incrementDigito (intToPeano 0) (intToPeano 0) x y) `shouldBeXy` [(intToPeano 1, intToPeano 0, [])]
    it "works on (8, 0)" $ do
      (solve $ incrementDigito (intToPeano 8) (intToPeano 0) x y) `shouldBeXy` [(intToPeano 9, intToPeano 0, [])]
    it "works on (9, 0)" $ do
      (solve $ incrementDigito (intToPeano 9) (intToPeano 0) x y) `shouldBeXy` [(intToPeano 0, intToPeano 1, [])]
    it "works on (0, 1)" $ do
      (solve $ incrementDigito (intToPeano 0) (intToPeano 1) x y) `shouldBeXy` [(intToPeano 1, intToPeano 1, [])]

  describe "sumDigitso" $ do
    it "works on (0, 0)" $ do
      (solve $ sumDigitso (intToPeano 0) (intToPeano 0) x y) `shouldBeXy` [(intToPeano 0, intToPeano 0, [])]
    it "works on (0, 2)" $ do
      (solve $ sumDigitso (intToPeano 0) (intToPeano 2) x y) `shouldBeXy` [(intToPeano 2, intToPeano 0, [])]
    it "works on (2, 0)" $ do
      (solve $ sumDigitso (intToPeano 2) (intToPeano 0) x y) `shouldBeXy` [(intToPeano 2, intToPeano 0, [])]
    it "works on (3, 4)" $ do
      (solve $ sumDigitso (intToPeano 3) (intToPeano 4) x y) `shouldBeXy` [(intToPeano 7, intToPeano 0, [])]
    it "works on (9, 1)" $ do
      (solve $ sumDigitso (intToPeano 9) (intToPeano 1) x y) `shouldBeXy` [(intToPeano 0, intToPeano 1, [])]
    it "works on (4, 8)" $ do
      (solve $ sumDigitso (intToPeano 4) (intToPeano 8) x y) `shouldBeXy` [(intToPeano 2, intToPeano 1, [])]

  describe "sumDigitso3" $ do
    it "works on (0, 0)" $ do
      (solve $ sumDigitso3 (intToPeano 0) (intToPeano 0) (intToPeano 0) x y) `shouldBeXy` [(intToPeano 0, intToPeano 0, [])]
    it "works on (3, 4, 5)" $ do
      (solve $ sumDigitso3 (intToPeano 3) (intToPeano 4) (intToPeano 5) x y) `shouldBeXy` [(intToPeano 2, intToPeano 1, [])]

  describe "sumNumberso" $ do
    it "computes 3+4" $ do
      (solutions 2 $ sumNumberso (intToPeano 3 -:- nil) (intToPeano 4 -:- nil)) `shouldBe` [(intToPeano 7 -:- nil, [])]
    it "computes 123+456" $ do
      (solutions 2 $ sumNumberso (hlistToList $ map intToPeano [3, 2, 1]) (hlistToList $ map intToPeano [6, 5, 4])) `shouldBe` [(hlistToList $ map intToPeano [9, 7, 5], [])]
    it "computes 456+187" $ do
      (solutions 2 $ sumNumberso (hlistToList $ map intToPeano [6, 5, 4]) (hlistToList $ map intToPeano [7, 8, 1])) `shouldBe` [(hlistToList $ map intToPeano [3, 4, 6], [])]
