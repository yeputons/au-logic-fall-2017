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

parseNumber :: String -> [Maybe Int]
parseNumber = map parseDigit
  where
    parseDigit '?' = Nothing
    parseDigit x   = Just $ digitToInt x

generateTerm :: [Maybe Int] -> State Int Term
generateTerm s = fmap hlistToList $ mapM generateDigit (reverse s)
  where
    generateDigit :: Maybe Int -> State Int Term
    generateDigit Nothing = do
      i <- get
      put (i + 1)
      return $ Var ("v" ++ show i)
    generateDigit (Just i) = return $ intToPeano i

parseInput :: String -> String -> String -> (Term, Term, Term)
parseInput a b c = evalState (parseInput' a b c) 0
  where
    parseInput' a b c = do
      at <- generateTerm (parseNumber a)
      bt <- generateTerm (parseNumber b)
      ct <- generateTerm (parseNumber c)
      return (at, bt, ct)

constructProblem :: (Term, Term, Term) -> Goal
constructProblem (a, b, c) =
  (Var "a" === a) &&& (Var "b" === b) &&& (Var "c" === c) &&& sumNumberso a b c

getSolution :: PSol -> (Term, Term, Term)
getSolution (PSol e n) = (fromJust $ lookup "a" e, fromJust $ lookup "b" e, fromJust $ lookup "c" e)

formatNumber :: Term -> String
formatNumber t = reverse $ map (intToDigit . peanoToInt) $ listToHlist t

formatSolution :: (Term, Term, Term) -> (String, String, String)
formatSolution (a, b, c) = (formatNumber a, formatNumber b, formatNumber c)

main :: IO ()
main = do
  let prob = constructProblem $ parseInput "89?" "?99" "98?"
  mapM_ (print . formatSolution . getSolution) $ solve prob
