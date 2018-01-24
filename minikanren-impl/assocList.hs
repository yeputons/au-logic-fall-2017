module AssocList (pair, lookupo) where
import Minikanren
import Lists

pair :: Term -> Term -> Term
pair a b = Func "," [a, b]

lookupo :: Term -> Term -> Term -> Goal
lookupo al k v =
    fresh $ \k' ->
    fresh $ \v' ->
    fresh $ \t ->
        (al === (pair k' v') -:- t) &&&
        ((k' === k &&& v' === v) |||
         (k' =/= k &&& lookupo t k v))
