import Test.Hspec
import Minikanren
import Peano
import Lists

x = Var "x"
y = Var "y"
z = Var "z"

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

  describe "lengtho" $ do
    it "calculates length of nil" $ (solutions 2 $ lengtho nil) `shouldBe` [noDiseq $ intToPeano 0]
    it "calculates length of x:nil" $ (solutions 2 $ lengtho (x-:-nil)) `shouldBe` [noDiseq $ intToPeano 1]
    it "calculates length of x:x:nil" $ (solutions 2 $ lengtho (x-:-x-:-nil)) `shouldBe` [noDiseq $ intToPeano 2]

  describe "appendo" $ do
    it "appends nil to nil" $ (solutions 2 $ appendo nil nil) `shouldBe` [noDiseq $ nil]
    it "appends nil to x:nil" $ (solutions 2 $ appendo nil (x-:-nil)) `shouldBe` [noDiseq $ x-:-nil]
    it "appends x:nil to nil" $ (solutions 2 $ appendo (x-:-nil) nil) `shouldBe` [noDiseq $ x-:-nil]
    it "appends x:nil to y:nil" $ (solutions 2 $ appendo (x-:-nil) (y-:-nil)) `shouldBe` [noDiseq $ x-:-y-:-nil]
    it "appends x:y:nil to z:y:nil" $ (solutions 2 $ appendo (x-:-y-:-nil) (z-:-y-:-nil)) `shouldBe` [noDiseq $ x-:-y-:-z-:-y-:-nil]

  describe "reverso" $ do
    it "reverses nil" $ (solutions 2 $ reverso $ nil) `shouldBe` [noDiseq $ nil]
    it "reverses x:nil" $ (solutions 2 $ reverso $ x-:-nil) `shouldBe` [noDiseq $ x-:-nil]
    it "reverses x:y:nil" $ (solutions 2 $ reverso $ x-:-y-:-nil) `shouldBe` [noDiseq $ y-:-x-:-nil]
    it "reverses x:y:z:nil" $ (solutions 2 $ reverso $ x-:-y-:-z-:-nil) `shouldBe` [noDiseq $ z-:-y-:-x-:-nil]
    it "reverses back x:y:z:nil" $ (solutions 1 $ \t -> reverso t $ x-:-y-:-z-:-nil) `shouldBe` [noDiseq $ z-:-y-:-x-:-nil]
