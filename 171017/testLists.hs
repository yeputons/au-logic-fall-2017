import Test.Hspec
import Minikanren
import Peano
import Lists

x = Var "x"

main :: IO ()
main = hspec $ do
  describe "lengtho predicate" $ do
    it "works on (nil, 0)" $ (isTrue $ lengtho nil (intToPeano 0)) `shouldBe` True
    it "works on (nil, 1)" $ (isTrue $ lengtho nil (intToPeano 1)) `shouldBe` False
    it "works on (x:nil, 0)" $ (isTrue $ lengtho (x-:-nil) (intToPeano 0)) `shouldBe` False
    it "works on (x:nil, 1)" $ (isTrue $ lengtho (x-:-nil) (intToPeano 1)) `shouldBe` True
    it "works on (x:nil, 2)" $ (isTrue $ lengtho (x-:-nil) (intToPeano 2)) `shouldBe` False
    it "works on (x:x:nil, 0)" $ (isTrue $ lengtho (x-:-x-:- nil) (intToPeano 0)) `shouldBe` False
    it "works on (x:x:nil, 1)" $ (isTrue $ lengtho (x-:-x-:- nil) (intToPeano 1)) `shouldBe` False
    it "works on (x:x:nil, 2)" $ (isTrue $ lengtho (x-:-x-:- nil) (intToPeano 2)) `shouldBe` True

    it "calculates length of nil" $ (solutions 2 $ lengtho nil) `shouldBe` [intToPeano 0]
    it "calculates length of x:nil" $ (solutions 2 $ lengtho (x-:-nil)) `shouldBe` [intToPeano 1]
    it "calculates length of x:x:nil" $ (solutions 2 $ lengtho (x-:-x-:-nil)) `shouldBe` [intToPeano 2]
