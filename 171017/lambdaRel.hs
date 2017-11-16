
compile :: Prog -> Term
compile (Var x) = Func x []
compile (Lambda x p) = Func ("\\" ++ x) [p]
compile (App x y) = Func "@" [x, y]
