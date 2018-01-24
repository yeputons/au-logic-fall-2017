import Task1
import Minikanren

main :: IO ()
main = do
  let prob = constructProblem $ parseInput "89?" "?99" "98?"
  mapM_ (print . formatSolution . getSolution) $ solve prob
