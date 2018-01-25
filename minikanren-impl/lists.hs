module Lists ((-:-), nil, lengtho, appendo, reverso, elemo, notElemo, hlistToList, listToHlist) where
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

appendo :: Term -> Term -> Term -> Goal
appendo a b ab =
   (a === nil &&& b === ab) |||
   (fresh $ \h ->
    fresh $ \t ->
    fresh $ \tb ->
    a === h -:- t &&& appendo t b tb &&& h -:- tb === ab)

reverso' :: Term -> Term -> Term -> Goal
reverso' a b rab =
  (a === nil &&& b === rab) |||
  (fresh $ \h ->
   fresh $ \t ->
   a === h -:- t &&& reverso' t (h -:- b) rab)

reverso :: Term -> Term -> Goal
reverso a ra = reverso' a nil ra

elemo :: Term -> Term -> Goal
elemo x xs = fresh $ \h -> fresh $ \t -> xs === h -:- t &&& (x === h ||| elemo x t)
  
notElemo :: Term -> Term -> Goal
notElemo x xs = xs === nil ||| (fresh $ \h -> fresh $ \t -> xs === h -:- t &&& (x =/= h &&& notElemo x t))

hlistToList :: [Term] -> Term
hlistToList []     = nil
hlistToList (x:xs) = x-:-hlistToList xs

listToHlist :: Term -> [Term]
listToHlist (Func "[]" []     ) = []
listToHlist (Func ":"  [x, xs]) = x : listToHlist xs
