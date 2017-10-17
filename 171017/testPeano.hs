import Test.Hspec
import Minikanren
import Peano

main :: IO ()
main = hspec $ do
  describe "eq predicate" $ do
    it "works on (0, 0)" $ do
      (isTrue $ eq (intToPeano 0) (intToPeano 0)) `shouldBe` True
    it "works on (0, 2)" $ do
      (isTrue $ eq (intToPeano 0) (intToPeano 2)) `shouldBe` False
    it "works on (2, 0)" $ do
      (isTrue $ eq (intToPeano 2) (intToPeano 0)) `shouldBe` False
    it "works on (3, 3)" $ do
      (isTrue $ eq (intToPeano 3) (intToPeano 3)) `shouldBe` True
    it "works on (2, 3)" $ do
      (isTrue $ eq (intToPeano 2) (intToPeano 3)) `shouldBe` False
    it "works on (3, 2)" $ do
      (isTrue $ eq (intToPeano 3) (intToPeano 2)) `shouldBe` False

  describe "add predicate" $ do
    it "works on (0, 0, 0)" $ do
      (isTrue $ add (intToPeano 0) (intToPeano 0) (intToPeano 0)) `shouldBe` True
    it "works on (0, 0, 1)" $ do
      (isTrue $ add (intToPeano 0) (intToPeano 0) (intToPeano 1)) `shouldBe` False
    it "works on (0, 1, 0)" $ do
      (isTrue $ add (intToPeano 0) (intToPeano 1) (intToPeano 0)) `shouldBe` False
    it "works on (1, 0, 0)" $ do
      (isTrue $ add (intToPeano 1) (intToPeano 0) (intToPeano 0)) `shouldBe` False
    it "works on (0, 1, 1)" $ do
      (isTrue $ add (intToPeano 0) (intToPeano 1) (intToPeano 1)) `shouldBe` True
    it "works on (1, 0, 1)" $ do
      (isTrue $ add (intToPeano 1) (intToPeano 0) (intToPeano 1)) `shouldBe` True
    it "works on (1, 1, 0)" $ do
      (isTrue $ add (intToPeano 1) (intToPeano 0) (intToPeano 0)) `shouldBe` False
    it "works on (1, 1, 1)" $ do
      (isTrue $ add (intToPeano 1) (intToPeano 1) (intToPeano 1)) `shouldBe` False
    it "works on (17, 23, 40)" $ do
      (isTrue $ add (intToPeano 17) (intToPeano 23) (intToPeano 40)) `shouldBe` True
    it "works on (17, 23, 41)" $ do
      (isTrue $ add (intToPeano 17) (intToPeano 23) (intToPeano 41)) `shouldBe` False

  describe "add" $ do
    it "adds 0 and 0" $ do
      (solutions 2 $ add (intToPeano 0) (intToPeano 0)) `shouldBe` [intToPeano 0]
    it "adds 0 and 1" $ do
      (solutions 2 $ add (intToPeano 0) (intToPeano 1)) `shouldBe` [intToPeano 1]
    it "adds 1 and 0" $ do
      (solutions 2 $ add (intToPeano 1) (intToPeano 0)) `shouldBe` [intToPeano 1]
    it "adds 1 and 1" $ do
      (solutions 2 $ add (intToPeano 1) (intToPeano 1)) `shouldBe` [intToPeano 2]
    it "adds 17 and 23" $ do
      (solutions 2 $ add (intToPeano 17) (intToPeano 23)) `shouldBe` [intToPeano 40]
    it "0+x=0" $ do
      (solutions 1 $ \x -> add (intToPeano 0) x (intToPeano 0)) `shouldBe` [intToPeano 0]
    it "x+0=0" $ do
      (solutions 1 $ \x -> add x (intToPeano 0) (intToPeano 0)) `shouldBe` [intToPeano 0]
    it "0+x=1" $ do
      (solutions 1 $ \x -> add (intToPeano 0) x (intToPeano 1)) `shouldBe` [intToPeano 1]
    it "x+0=1" $ do
      (solutions 1 $ \x -> add x (intToPeano 0) (intToPeano 1)) `shouldBe` [intToPeano 1]
    it "1+x=1" $ do
      (solutions 1 $ \x -> add (intToPeano 1) x (intToPeano 1)) `shouldBe` [intToPeano 0]
    it "x+1=1" $ do
      (solutions 1 $ \x -> add x (intToPeano 1) (intToPeano 1)) `shouldBe` [intToPeano 0]
    it "17+x=40" $ do
      (solutions 1 $ \x -> add (intToPeano 17) x (intToPeano 40)) `shouldBe` [intToPeano 23]
    it "x+23=40" $ do
      (solutions 1 $ \x -> add x (intToPeano 23) (intToPeano 40)) `shouldBe` [intToPeano 17]
