module Lists ((-:-), nil, lengtho) where
import Minikanren
import Peano

nil :: Term
nil = Func "[]" []

infixr 6 -:-

(-:-) :: Term -> Term -> Term
(-:-) a b = Func ":" [a, b]

lengtho :: Term -> Term -> Goal
lengtho a l =
   (a === nil &&& l === o) |||
   (fresh $ \h ->
    fresh $ \t ->
    fresh $ \l' ->
    a === h -:- t &&& lengtho t l' &&& l === s l')
