module Task02 (Coin(..), getSum, getOptimalSplit, solve) where
import Data.List

data Coin = E2 | E1 | C50 | C20 | C10 deriving (Show, Eq, Enum, Bounded)

toCents E2 = 200
toCents E1 = 100
toCents C50 = 50
toCents C20 = 20
toCents C10 = 10

getSum = sum . map toCents

getOptimalSplit 0 = []
getOptimalSplit x =
  let Just c = find (\c -> x >= (toCents c)) [minBound..maxBound] in
  c : getOptimalSplit (x - (toCents c))

solve = getOptimalSplit . getSum
