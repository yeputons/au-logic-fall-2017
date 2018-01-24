module Task1 where
import Minikanren
import Peano

base :: Term
base = intToPeano 10

incrementDigito :: Term -> Term -> Term -> Term -> Goal
incrementDigito x x' y y' =
  (s x === base &&& y === o &&& y' === s x') |||
  (s x =/= base &&& y === s x &&& y' === x')

sumDigitso :: Term -> Term -> Term -> Term -> Goal
sumDigitso a b r r' =
  ((a === o) &&& (b === r) &&& (r' === o)) |||
  (fresh $ \a' -> (a === s a') &&& (
    fresh $ \q -> fresh $ \q' -> sumDigitso a' b q q' &&& incrementDigito q q' r r'
  ))
