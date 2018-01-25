import Test.Hspec
import Minikanren
import Peano

a = Func "a" []
b = Func "b" []
c = Func "c" []
d = Func "d" []

x = Var "x"
y = Var "y"
z = Var "z"

main :: IO ()
main = hspec $ do
  describe "disequality constraint" $ do
    it "a =/= a" $ (isTrue $ a =/= a) `shouldBe` False
    it "a =/= b" $ (isTrue $ a =/= b) `shouldBe` True
    it "a =/= x" $ (isTrue $ a =/= x) `shouldBe` True
    it "a === x &&& a =/= x" $ (isTrue $ a === x &&& a =/= x) `shouldBe` False
    it "a =/= x &&& a === x" $ (isTrue $ a =/= x &&& a === x) `shouldBe` False
    it "b === x &&& a =/= x" $ (solutions 2 $ \x -> b === x &&& a =/= x) `shouldBe` [noDiseq b]
    it "a =/= x &&& b === x" $ (solutions 2 $ \x -> a =/= x &&& b === x) `shouldBe` [noDiseq b]
    it "a === x &&& b =/= x" $ (solutions 2 $ \x -> a === x &&& b =/= x) `shouldBe` [noDiseq a]
    it "b =/= x &&& a === x" $ (solutions 2 $ \x -> b =/= x &&& a === x) `shouldBe` [noDiseq a]

  describe "noSolutions" $ do
    it "noSolutions (a === a)" $ (isTrue $ noSolutions (a === a)) `shouldBe` False
    it "noSolutions (a =/= a)" $ (isTrue $ noSolutions (a =/= a)) `shouldBe` True
    it "noSolutions (a =/= x)" $ (isTrue $ noSolutions (a =/= x)) `shouldBe` False
    it "a === x &&& noSolutions (a =/= x)" $ (isTrue $ a === x &&& noSolutions (a =/= x)) `shouldBe` True
    it "a =/= x &&& noSolutions (a === x)" $ (isTrue $ a =/= x &&& noSolutions (a === x)) `shouldBe` True
