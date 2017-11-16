import Test.Hspec
import Lambda
import LambdaProg

main :: IO ()
main = hspec $ do
  describe "church" $ do
    it "calculates 2" $ (compute $ church 0 :@: Ref "f" :@: Ref "z") `shouldBe` (Ref "z")
    it "calculates 2" $ (compute $ church 2 :@: Ref "f" :@: Ref "z") `shouldBe` (Ref "f" :@: (Ref "f" :@: Ref "z"))
