module Task1 where
import Minikanren
import Peano
import Lists

base :: Term
base = intToPeano 10

incrementDigito :: Term -> Term -> Term -> Term -> Goal
incrementDigito x x' y y' =
  (s x === base &&& y === o &&& y' === s x') |||
  (s x =/= base &&& y === s x &&& y' === x')

addDigito :: Term -> Term -> Term -> Term -> Term -> Goal
addDigito a x x' y y' =
  (a === o &&& x === y &&& x' === y') |||
  (fresh $ \a' -> (a === s a') &&& (
    fresh $ \q -> fresh $ \q' -> addDigito a' x x' q q' &&& incrementDigito q q' y y'
  ))

sumDigitso :: Term -> Term -> Term -> Term -> Goal
sumDigitso a b r r' = addDigito a b o r r'

sumDigitso3 :: Term -> Term -> Term -> Term -> Term -> Goal
sumDigitso3 a b c r r' = fresh $ \q -> fresh $ \q' ->
  sumDigitso a b q q' &&& addDigito c q q' r r'

sumNumberso' :: Term -> Term -> Term -> Term -> Goal
sumNumberso' a b c s =
  (a === nil &&& b === nil &&& c === o &&& s === nil) |||
  (fresh $ \ah -> fresh $ \at -> a === ah-:-at &&& (
  (fresh $ \bh -> fresh $ \bt -> b === bh-:-bt &&& (
  (fresh $ \sh -> fresh $ \st -> s === sh-:-st &&& (
    fresh $ \c' ->
    sumDigitso3 ah bh c sh c' &&& sumNumberso' at bt c' st
  ))
  ))
  ))

sumNumberso :: Term -> Term -> Term -> Goal
sumNumberso a b s = sumNumberso' a b o s
