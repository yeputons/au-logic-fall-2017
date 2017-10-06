import Data.List
import Data.List.Unique

data Formula = Var String
             | Not Formula
             | Formula :/\: Formula
             | Formula :\/: Formula
             | Formula :=>: Formula
             | Formula :<=: Formula
             | Formula :<=>: Formula
             deriving (Show, Eq)

hom f (Var x)    = Var x
hom f (Not x)    = Not (f x)
hom f (a :/\: b) = (f a) :/\: (f b)
hom f (a :\/: b) = (f a) :\/: (f b)
hom f (a :=>: b) = (f a) :=>: (f b)
hom f (a :<=: b) = (f a) :<=: (f b)
hom f (a :<=>: b) = (f a) :<=>: (f b)

toVnoa (a :=>: b) = toVnoa $ (Not a) :\/: b
toVnoa (a :<=: b) = toVnoa $ a :\/: (Not b)
toVnoa (a :<=>: b) = toVnoa $ (a :/\: b) :\/: ((Not a) :/\: (Not b))
toVnoa x = hom toVnoa x

propagateNot x@(Not (Var _)) = x
propagateNot (Not (a :/\: b)) = hom propagateNot ((Not a) :\/: (Not b))
propagateNot (Not (a :\/: b)) = hom propagateNot ((Not a) :/\: (Not b))
propagateNot (Not (Not a)) = propagateNot a
propagateNot (Not _) = undefined
propagateNot x = hom propagateNot x

data CnfVar = Pos String | Neg String deriving (Show, Eq, Ord)
type CnfClause = [CnfVar]
type CnfFormula = [CnfClause]

toCnf = toCnf' . propagateNot . toVnoa

toCnf' (Var x) = [[Pos x]]
toCnf' (Not (Var x)) = [[Neg x]]
toCnf' (a :/\: b) = (toCnf' a) ++ (toCnf' b)
toCnf' (a :\/: b) =
  let a' = toCnf' a in
  let b' = toCnf' b in
  [a'' ++ b'' | a'' <- a', b'' <- b']

getName (Pos x) = x
getName (Neg x) = x

varNeg (Pos x) = Neg x
varNeg (Neg x) = Pos x

-- DPLL
dropDuplicates :: CnfFormula -> CnfFormula
dropDuplicates = map sortUniq

dropAlwaysTrue :: CnfFormula -> CnfFormula
dropAlwaysTrue = filter (not . alwaysTrue)
  where
    alwaysTrue cs = any (\c -> (varNeg c) `elem` cs) cs

dropSameSign :: CnfFormula -> CnfFormula
dropSameSign cs =
  let vars = [v | v <- concat cs, isSameSign v cs] in
  [c | c <- cs, c `intersect` vars == []]

isSameSign :: CnfVar -> CnfFormula -> Bool
isSameSign v cs = all (\c -> not $ elem (varNeg v) c) cs

unitProp :: CnfVar -> CnfFormula -> CnfFormula
unitProp _ [] = []
unitProp v (c:cs) =
  let hasp = v `elem` c in
  let hasn = (varNeg v) `elem` c in
  if hasn then
    [[]]
  else
    (if hasp then [] else [c]) ++ unitProp v cs
