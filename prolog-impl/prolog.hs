import Data.List

data Term = Func String [Term] | Var String
data ClauseItem = Pos Term | Neg Term
type Clause = [ClauseItem]

instance Show Term where
  showsPrec d (Var x) = (x++)
  showsPrec d (Func n []) = (n++)
  showsPrec d (Func n ts) =
      (n++) . ('(':) . (ts'++)  . (")"++)
    where
      ts' = intercalate ", " (map show ts)

instance Show ClauseItem where
  showsPrec d (Pos t) = showsPrec d t
  showsPrec d (Neg t) =
    ("!"++) . (showsPrec d t)

-- TODO

-- Prolog Program

o = Func "o" []
s x = Func "s" [x]
add x y z = Func "add" [x, y, z]

add1 = [Pos $ add o (Var "X") (Var "X")]
add2 = [Pos $ add (s $ Var "X") (Var "Y") (Var "Z"),
        Neg $ add (Var "X") (s $ Var "Y") (Var "Z")]
q1 = add (s o) (s o) (Var "T")

main = do
  print add1
  print add2
  print q1
