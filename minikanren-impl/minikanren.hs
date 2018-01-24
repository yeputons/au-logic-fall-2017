module Minikanren (Term(Func, Var), Goal, (===), (&&&), (|||), fresh, isTrue, noDiseq, solve, solutions, run) where

import Data.List
import Data.Maybe
import Control.Monad

data Term = Func String [Term] | Var String deriving (Show, Eq)
type Subst = [(String, Term)]
data PSol = PSol Subst Subst  -- equality, disequality
type Solution = (Term, Subst)  -- term, disequality for variables

unify :: Term -> Term -> Subst -> Maybe Subst
unify (Var x) y s | Just x' <- lookup x s = unify x' y s
unify x (Var y) s | Just y' <- lookup y s = unify x y' s

unify (Var x) (Var y) s | x == y = Just []
unify (Var x) y s | isPrefixOf "_" x = Just $ [(x, y)]
unify x (Var y) s | isPrefixOf "_" y = Just $ [(y, x)]
unify (Var x) y s = Just $ [(x, y)]
unify x (Var y) s = Just $ [(y, x)]
unify (Func x xs) (Func y ys) s | x == y && length xs == length ys = foldM f [] (zip xs ys)
  where
    f :: Subst -> (Term, Term) -> Maybe Subst
    f s' (a, b) = do
      s'' <- (unify a b (s' ++ s))
      return $ s' ++ s''
unify (Func _ _)  (Func _ _) _  | otherwise = Nothing

type State = (PSol, Int)
type Goal = State -> [State]  -- Computes all solutions

infix 4 ===
infixr 3 &&&
infixr 2 |||

(===) :: Term -> Term -> Goal
(===) a b (PSol e d, v) = do
    e' <- maybeToList (unify a b e)
    let e'' = e' ++ e
    d'' <- maybeToList $ msum $ Just [] : map (updateDiseq e'') d
    return (PSol (e' ++ e) d, v)

updateDiseq :: Subst -> (String, Term) -> Maybe Subst
updateDiseq e (n, d) =
    case unify (Var n) d e of
    Nothing -> Just []
    Just [] -> Nothing
    Just x  -> Just x

(&&&) :: Goal -> Goal -> Goal
(&&&) a b s = do
    s' <- a s
    s'' <- b s'
    return s''

(|||) :: Goal -> Goal -> Goal
(|||) a b s = (a s) ++ (b s)

fresh :: (Term -> Goal) -> Goal
fresh f (s, v) =
    let vname = "_" ++ (show v) in
    f (Var vname) (s, v + 1)

reify :: Subst -> Term -> Term
reify s (Var x') | Just y <- lookup x' s  = reify s y
reify s x@(Var _) | otherwise = x
reify s (Func f xs) = Func f (map (reify s) xs)

solve :: Goal -> [PSol]
solve g = map fst $ g (PSol [] [], 0)

isTrue :: Goal -> Bool
isTrue = not . null . solve

noDiseq :: Term -> Solution
noDiseq x = (x, [])

solutions :: Int -> (Term -> Goal) -> [Solution]
solutions c g =
  let vtop = "_top" in
  let g' = g (Var vtop) in
  take c $ map (psol2Sol vtop) (solve g')
  where
    psol2Sol :: String -> PSol -> Solution
    psol2Sol vtop (PSol e d) = (reify e (fromJust $ lookup vtop e), d)

run :: (Term -> String) -> Int -> (Term -> Goal) -> IO ()
run p c g =
  mapM_ (putStrLn . (showSolution p)) (solutions c g)

showSolution :: (Term -> String) -> Solution -> String
showSolution p (t, []) = p t
showSolution p (t, d ) = p t ++ "[" ++ (concat $ intersperse ", " $ map showDiseq d) ++ "]"
  where
    showDiseq (v, d) = v ++ "=/=" ++ p d
