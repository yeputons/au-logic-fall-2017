{-# LANGUAGE TemplateHaskell #-}

import Chr
import Test.Hspec
import Data.List

heat = Func "heat" []
electricity = Func "electricity" []
h2 = Func "h2" []
o2 = Func "o2" []
h2o = Func "h2o" []

rules =
  [
    [heat, h2, h2, o2] <=> [h2o, h2o],
    [electricity, h2o, h2o] <=> [h2, h2, o2]
  ]

a `shouldBeNoOrd` b = (fmap sort a) `shouldBe` (fmap sort b)

main = do
  hspec $ describe "Task03" $ do
    it "works on empty test" $ do
      (eval rules []) `shouldBeNoOrd` (Just [])
    it "works on some examples" $ do
      (eval rules [
        electricity, electricity,
        heat, heat, heat, heat, heat,
        h2, h2, h2, h2, h2,
        o2, o2, o2, o2, o2,
        h2o, h2o, h2o, h2o, h2o
       ]) `shouldBeNoOrd` (Just [
        heat, h2, o2, o2, o2,
        h2o, h2o, h2o, h2o, h2o, h2o, h2o, h2o, h2o
       ])
