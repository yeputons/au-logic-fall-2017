module Chr where
import Data.List
import Data.Maybe
import Control.Monad

data Term = Func String [Term] | Var String deriving (Eq, Ord)
instance Show Term where
  showsPrec _ (Func x xs) =
    (x ++).('(':).(
      foldr (.) id $ intersperse (", "++) (map (showsPrec 0) xs)
    ).(')':)
  showsPrec _ (Var x) = (x++)

type TermMultiset = [Term]

data Rule = Simplification TermMultiset TermMultiset
          | Propagation TermMultiset TermMultiset
instance Show Rule where
  showsPrec _ (Simplification h b) = (showsPrec 0 h).(" <=> "++).(showsPrec 0 b)
  showsPrec _ (Propagation h b) = (showsPrec 0 h).(" ==> "++).(showsPrec 0 b)


false = Func "false" []
true = Func "true" []
h <=> b = Simplification h b
h ==> b = Propagation h b

type Subst = [(String, Term)]

reify :: Subst -> Term -> Term
reify s (Var x') | Just y <- lookup x' s = y
reify s x@(Var _) | otherwise = x
reify s (Func f xs) = Func f (map (reify s) xs)

eval :: [Rule] -> TermMultiset -> Maybe TermMultiset
eval rs g | false `elem` g = Nothing
eval rs g =
  let g' = filter (/= true) g in
  case evalStep rs g of
  Just g' -> eval rs g'
  Nothing -> Just g'

evalStep :: [Rule] -> TermMultiset -> Maybe TermMultiset
evalStep rs g = msum $ map (tryRule g) rs

tryRule :: TermMultiset -> Rule -> Maybe TermMultiset
tryRule g (Simplification h b) = do
  (g', s) <- matchMulti g h []
  return $ (map (reify s) b) ++ g'
tryRule g (Propagation h b) = do
  (_, s) <- matchMulti g h []
  return $ (map (reify s) b) ++ g

matchMulti :: TermMultiset -> TermMultiset -> Subst -> Maybe (TermMultiset, Subst)
matchMulti gs [] s = Just (gs, s)
matchMulti gs (r:rs') s | r == true = matchMulti gs rs' s
matchMulti gs (r:rs') s | otherwise = msum $ map matchMulti' (enumerateWithDelete gs)
  where
    matchMulti' (g, gs') =
      (match g r s) >>= (matchMulti gs' rs')

match :: Term -> Term -> Subst -> Maybe Subst
match x (Var y) s | Just y' <- lookup y s = match x y' s
match x (Var y) s | otherwise             = Just $ (y, x):s
match (Var x) (Func _ _) _                = Nothing
match (Func x xs) (Func y ys) s | x == y && length xs == length ys = foldM f s (zip xs ys)
  where
    f :: Subst -> (Term, Term) -> Maybe Subst
    f s (a, b) = match a b s
match (Func _ _ ) (Func _ _ ) _ | otherwise = Nothing

enumerateWithDelete :: [a] -> [(a, [a])]
enumerateWithDelete xs = zip xs (map (uncurry (++)) $ zip (inits xs) (tail $ tails xs))
