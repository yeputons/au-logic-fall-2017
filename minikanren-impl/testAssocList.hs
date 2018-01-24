import Test.Hspec
import Minikanren
import Peano
import Lists
import AssocList

a = intToPeano 1
b = intToPeano 2
c = intToPeano 3
d = intToPeano 4

ab = pair a b
ac = pair a c
ad = pair a d
cd = pair c d

main :: IO ()
main = hspec $ do
  describe "lookupo predicate" $ do
    it "works on nil" $ (isTrue $ lookupo nil a b) `shouldBe` False
    it "works on (ab:nil, a, b)" $ (isTrue $ lookupo (ab-:-nil) a b) `shouldBe` True
    it "works on (ab:nil, a, c)" $ (isTrue $ lookupo (ab-:-nil) a c) `shouldBe` False
    it "works on (ab:nil, b, b)" $ (isTrue $ lookupo (ab-:-nil) b b) `shouldBe` False
    it "works on (ab:nil, b, a)" $ (isTrue $ lookupo (ab-:-nil) b a) `shouldBe` False
    it "works on (ab:ac:nil, a, b)" $ (isTrue $ lookupo (ab-:-ac-:-nil) a b) `shouldBe` True
    it "works on (ab:ac:nil, a, c)" $ (isTrue $ lookupo (ab-:-ac-:-nil) a c) `shouldBe` False
    it "works on (ab:ac:nil, b, b)" $ (isTrue $ lookupo (ab-:-ac-:-nil) b b) `shouldBe` False
    it "works on (ab:ac:nil, b, a)" $ (isTrue $ lookupo (ab-:-ac-:-nil) b a) `shouldBe` False
    it "works on (cd:ab:ac:nil, a, b)" $ (isTrue $ lookupo (cd-:-ab-:-ac-:-nil) a b) `shouldBe` True
    it "works on (cd:ab:ac:nil, a, c)" $ (isTrue $ lookupo (cd-:-ab-:-ac-:-nil) a c) `shouldBe` False
    it "works on (cd:ab:ac:nil, a, c)" $ (isTrue $ lookupo (cd-:-ab-:-ac-:-nil) c d) `shouldBe` True

  describe "lookupo value" $ do
    it "works on nil" $ (solutions 1 $ lookupo nil a) `shouldBe` []
    it "works on (ab:nil, a)" $ (solutions 2 $ lookupo (ab-:-nil) a) `shouldBe` [noDiseq b]
    it "works on (ab:ac:nil, a)" $ (solutions 2 $ lookupo (ab-:-ac-:-nil) a) `shouldBe` [noDiseq b]
    it "works on (cd:ab:ac:nil, a)" $ (solutions 2 $ lookupo (cd-:-ab-:-ac-:-nil) a) `shouldBe` [noDiseq b]
    it "works on (cd:ab:ac:nil, c)" $ (solutions 2 $ lookupo (cd-:-ab-:-ac-:-nil) c) `shouldBe` [noDiseq d]

  describe "lookupo key" $ do
    it "works on nil" $ (solutions 1 $ \k -> lookupo nil k a) `shouldBe` []
    it "works on (ab:nil, b)" $ (solutions 2 $ \k -> lookupo (ab-:-nil) k b) `shouldBe` [noDiseq a]
    it "works on (ab:ac:nil, b)" $ (solutions 2 $ \k -> lookupo (ab-:-ac-:-nil) k b) `shouldBe` [noDiseq a]
    it "works on (cd:ab:ac:nil, b)" $ (solutions 2 $ \k -> lookupo (cd-:-ab-:-ac-:-nil) k b) `shouldBe` [noDiseq a]
    it "works on (cd:ab:ac:nil, d)" $ (solutions 2 $ \k -> lookupo (cd-:-ab-:-ac-:-nil) k d) `shouldBe` [noDiseq c]
    it "works on (ad:cd:ab:ac:nil, d)" $ (solutions 2 $ \k -> lookupo (ad-:-cd-:-ab-:-ac-:-nil) k d) `shouldBe` [noDiseq a, noDiseq c]
    it "works on (ab:ad:cd:ab:ac:nil, d)" $ (solutions 2 $ \k -> lookupo (ab-:-ad-:-cd-:-ab-:-ac-:-nil) k d) `shouldBe` [noDiseq c]

  describe "lookupo list" $ do
    it "works on nil" $ (solutions 1 $ lookupo nil a) `shouldBe` []
    it "works on (?:nil, a, b)" $ (solutions 2 $ \x -> lookupo (x-:-nil) a b) `shouldBe` [noDiseq ab]
    it "works on (cd:?:nil, a, b)" $ (solutions 2 $ \x -> lookupo (cd-:-x-:-nil) a b) `shouldBe` [noDiseq ab]
    it "works on (cd:?:ac:nil, a, b)" $ (solutions 2 $ \x -> lookupo (cd-:-x-:-ac-:-nil) a b) `shouldBe` [noDiseq ab]
