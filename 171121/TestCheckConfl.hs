import CheckConfl
import Test.Hspec
import Data.List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

main = do
  hspec $ do
    describe "collectNames" $ do
      it "collects names of variables" $ do
        (collectVarNames (Func "foo" [Func "foo" [Var "x", Var "y"], Func "bar" [Var "y", Var "y"], Var "z"]))
        `shouldBe` (Set.fromList ["x", "y", "z"])
    
    describe "generateName" $ do
      it "preserves non-bad name" $ do
        (generateName "foo" (Set.fromList ["foo1", "foo2"])) `shouldBe` "foo"
      it "generates non-bad name" $ do
        (generateName "foo" (Set.fromList ["foo", "foo2"])) `shouldBe` "foo0"
      it "generates non-bad name even if the first attempt fails" $ do
        (generateName "foo" (Set.fromList ["foo", "foo0", "foo1", "foo2"])) `shouldBe` "foo3"

    describe "generateNames" $ do
      it "preserves non-bad names" $ do
        (generateNames (Set.fromList ["foo", "bar"]) (Set.fromList ["foo1", "foo2"]))
        `shouldBe` (Map.fromList [("foo", "foo"), ("bar", "bar")])
      it "generates non-bad names" $ do
        (generateNames (Set.fromList ["foo", "bar"]) (Set.fromList ["foo", "bar"]))
        `shouldBe` (Map.fromList [("foo", "foo0"), ("bar", "bar0")])
      it "generates non-bad names even if they intersect" $ do
        (generateNames (Set.fromList ["foo", "foo0"]) (Set.fromList ["foo", "bar"]))
        `shouldBe` (Map.fromList [("foo", "foo1"), ("foo0", "foo0")])

    describe "renameVars" $ do
      it "preserves non-bad vars" $ do
        (renameVars (Func "foo" [Func "foo" [Var "x", Var "y"], Var "z"]) Set.empty)
        `shouldBe`
        (Func "foo" [Func "foo" [Var "x", Var "y"], Var "z"])
      it "renames bad vars" $ do
        (renameVars (Func "foo" [Func "foo" [Var "x", Var "y"], Var "z"]) (Set.fromList ["foo", "x", "x0", "y"]))
        `shouldBe`
        (Func "foo" [Func "foo" [Var "x1", Var "y0"], Var "z"])
