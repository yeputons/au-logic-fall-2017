module Task1 where
import Data.Char
import Data.Maybe
import Control.Monad.State
import Minikanren
import Peano
import Lists

base :: Term
base = intToPeano 10

ltBase :: Term -> Goal
ltBase t = foldr (|||) (t === o) [t === intToPeano x | x <- [1..9]]

incrementDigito :: Term -> Term -> Term -> Term -> Goal
incrementDigito x x' y y' =
  (s x === base &&& y === o &&& y' === s x') |||
  (s x =/= base &&& y === s x &&& y' === x')

addDigito :: Term -> Term -> Term -> Term -> Term -> Goal
addDigito a x x' y y' = ltBase a &&& (
  (a === o &&& x === y &&& x' === y') |||
  (fresh $ \a' -> (a === s a') &&& (
    fresh $ \q -> fresh $ \q' -> addDigito a' x x' q q' &&& incrementDigito q q' y y'
  ))
  )

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

placeholder = Func "?" []

matcho :: Term -> Term -> Goal
matcho p n =
  (p === nil &&& n === nil) |||
  (fresh $ \ph -> fresh $ \pt -> p === ph -:- pt &&&
  (fresh $ \nh -> fresh $ \nt -> n === nh -:- nt &&&
    ((ph =/= placeholder &&& ph === nh) |||
     (ph === placeholder &&& ltBase nh)
    ) &&&
    matcho pt nt
  )
  )

solutiono :: Term -> Term -> Term -> Term -> Term -> Term -> Goal
solutiono pa pb pc a b c =
  matcho pa a &&& matcho pb b &&& matcho pc c &&& sumNumberso a b c

parsePattern :: String -> Term
parsePattern s = hlistToList $ reverse $ map parseDigit s
  where
    parseDigit '?' = placeholder
    parseDigit x   = intToPeano $ digitToInt x

constructProblem :: Term -> Term -> Term -> Goal
constructProblem pa pb pc = solutiono pa pb pc (Var "a") (Var "b") (Var "c")

getSolution :: PSol -> (Term, Term, Term)
getSolution (PSol e n) = (fromJust $ lookup "a" e, fromJust $ lookup "b" e, fromJust $ lookup "c" e)

formatPattern :: Term -> String
formatPattern t = map formatDigit $ reverse $ listToHlist t
  where
    formatDigit (Func "?" []) = '?'
    formatDigit n = intToDigit $ peanoToInt n

formatSolution :: (Term, Term, Term) -> (String, String, String)
formatSolution (a, b, c) = (formatPattern a, formatPattern b, formatPattern c)
