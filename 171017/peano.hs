module Peano (o, s, intToPeano, peanoToInt, eq, add, mul) where
import Minikanren

o :: Term
o = Func "o" []

s :: Term -> Term
s x = Func "s" [x]

intToPeano :: Int -> Term
intToPeano 0 = o
intToPeano x | x > 0 = s $ intToPeano $ x - 1

peanoToInt :: Term -> Int
peanoToInt (Func "o" []) = 0
peanoToInt (Func "s" [x]) = 1 + peanoToInt x

peanoPrinter :: Term -> String
peanoPrinter = show . peanoToInt

eq :: Term -> Term -> Goal
eq a b =
  ((a === o) &&& (b === o)) |||
  (fresh $ \x -> fresh $ \y -> (a === s x) &&& (b === s y) &&& (eq x y))

add :: Term -> Term -> Term -> Goal
add a b r =
  ((a === o) &&& (b === r)) |||
  (fresh $ \a' -> (a === s a') &&& (add a' (s b) r))

mul a b r =
  ((a === o) &&& (r === o)) |||
  (fresh $ \a' -> (a === s a') &&& (fresh $ \r' -> mul a' b r' &&& add b r' r))
