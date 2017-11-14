{-# LANGUAGE TemplateHaskell #-}

import Chr
import Test.Hspec
import Data.List

nil = Func "nil" []
a -:- b = Func "cons" [a, b]
rev x = Func "rev" [x]
rev2 x y = Func "rev2" [x, y]
infixr 5 -:-

varX = Var "X"
varXs = Var "Xs"
varY = Var "Y"
varZ = Var "Z"
valOf x = Func (show x) []

rules =
  [
    [rev varX] <=> [rev2 varX nil],
    [rev2 nil varX] <=> [varX],
    [rev2 (varX -:- varXs) varY] <=> [rev2 varXs (varX -:- varY)]
  ]

a `shouldBeNoOrd` b = (fmap sort a) `shouldBe` (fmap sort b)

main = do
  hspec $ describe "Task04" $ do
    it "works on nil" $ do
      (eval rules [rev $ nil]) `shouldBeNoOrd` (Just [nil])
    it "works on 1:nil" $ do
      (eval rules [rev $ valOf 1 -:- nil]
       ) `shouldBeNoOrd` (Just [valOf 1 -:- nil])
    it "works on 1:2:nil" $ do
      (eval rules [rev $ valOf 1 -:- (valOf 2 -:- nil)]
       ) `shouldBeNoOrd` (Just [valOf 2 -:- (valOf 1 -:- nil)])
    it "works on 1:2:3:nil" $ do
      (eval rules [rev $ valOf 1 -:- (valOf 2 -:- (valOf 3 -:- nil))]
       ) `shouldBeNoOrd` (Just [valOf 3 -:- (valOf 2 -:- (valOf 1 -:- nil))])
    it "works on X:Y:Xs:X:nil" $ do
      (eval rules [rev $ varX -:- (varY -:- (varXs -:- (varX -:- (varX -:- (varZ -:- nil)))))]
       ) `shouldBeNoOrd` (Just [varZ -:- (varX -:- (varX -:- (varXs -:- (varY -:- (varX -:- nil)))))])
