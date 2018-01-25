import Task1
import Minikanren
import Peano
import Lists

main :: IO ()
main = do
  let pa = parsePattern "89?"
  let pb = parsePattern "?99"
  let pc = parsePattern "98?"
  let prob = constructProblem pa pb pc
  mapM_ (print . formatSolution . getSolution) $ solve prob

  let prob = ssprob (intToPeano 3) (Var "a") (Var "b") (Var "c")
  mapM_ (print . formatSolution . getSolution) $ solve prob
