import Data.List

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

data CnfVar = Pos String | Neg String deriving (Show, Eq)
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

-- DPP (Dans-Puthem Procedure)
dpp [] = True
dpp ([]:_) = False
dpp cs@((v:_):_) =
  let vname = getName v in
  let (pos, neg, r) = dpp_collect vname cs in
  if (null pos) || (null neg) then dpp r
  else dpp $ r ++ [delete (Pos vname) p ++ delete (Neg vname) n | p <- pos, n <- neg]

dpp_collect _ [] = ([], [], [])
dpp_collect vname (c:cs) = 
  let hasp = (Pos vname) `elem` c in
  let hasn = (Neg vname) `elem` c in
  let (pos, neg, r) = (dpp_collect vname cs) in
  if hasp && hasn then (pos, neg, r)
  else if hasp then (c:pos, neg, r)
  else if hasn then (pos, c:neg, r)
  else (pos, neg, c:r)
