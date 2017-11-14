import Chr
import Test.Hspec
import Data.List

north = Func "North" []
east = Func "East" []
west = Func "West" []
south = Func "South" []

rules =
  [
    [east, west] <=> [true],
    [north, south] <=> [true]
  ]

a `shouldBeNoOrd` b = (fmap sort a) `shouldBe` (fmap sort b)

main = do
  hspec $ describe "Task01" $ do
    it "works on empty test" $ do
      (eval rules []) `shouldBeNoOrd` (Just [])
    it "works on some examples" $ do
      (eval rules [north, east, west, south]) `shouldBeNoOrd` (Just [])
      (eval rules [north, east, south, east, north]) `shouldBeNoOrd` (Just [north, east, east])
