module CheckConfl where
import Data.List
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map.Strict as Map

data Term = Func String [Term] | Var String deriving (Eq, Ord)
instance Show Term where
  showsPrec _ (Func x xs) =
    (x ++).('(':).(
      foldr (.) id $ intersperse (", "++) (map (showsPrec 0) xs)
    ).(')':)
  showsPrec _ (Var x) = (x++)

type Rule = (Term, Term)
type Subst = Map String Term

reify :: Subst -> Term -> Term
reify s (Var x')  | Just y <- Map.lookup x' s = y
reify s x@(Var _) | otherwise = x
reify s (Func f xs) = Func f (map (reify s) xs)

collectVarNames :: Term -> Set String
collectVarNames (Var x) = Set.singleton x
collectVarNames (Func _ xs) = Set.unions $ map collectVarNames xs

generateName :: String -> Set String -> String
generateName n bad | Set.notMember n bad = n
generateName n bad | otherwise           = generateName' n bad 0
  where
    generateName' n bad i | Set.notMember (n ++ (show i)) bad = n ++ show i
    generateName' n bad i | otherwise                         = generateName' n bad (i + 1)

generateNames :: Set String -> Set String -> Map String String
generateNames xs bad = Set.foldr step Map.empty xs
  where
    step :: String -> Map String String -> Map String String
    step x names =
      let bad' = bad `Set.union` (Map.keysSet names) in
      Map.insert x (generateName x bad') names

rename :: Term -> Map String String -> Term
rename t ns = reify (Map.map Var ns) t

renameVars :: Term -> Set String -> Term
renameVars t bad = rename t (generateNames (collectVarNames t) bad)

unify :: Term -> Term -> Subst -> Maybe Subst
unify (Var x) y s | Just x' <- Map.lookup x s = unify x' y s
unify x (Var y) s | Just y' <- Map.lookup y s = unify x y' s

unify (Var x) (Var y) s | x == y = Just s
unify (Var x) y s = Just $ Map.insert x y s
unify x (Var y) s = Just $ Map.insert y x s
unify (Func x xs) (Func y ys) s | x == y && length xs == length ys = foldM f s (zip xs ys)
  where
    f :: Subst -> (Term, Term) -> Maybe Subst
    f s (a, b) = unify a b s
unify (Func _ _)  (Func _ _) _  | otherwise = Nothing
