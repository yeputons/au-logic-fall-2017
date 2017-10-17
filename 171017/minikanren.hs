module Minikanren (Term(Func, Var), Goal, (===), (&&&), (|||), fresh, isTrue, solutions, run) where

import Data.List
import Data.Maybe
import Control.Monad

data Term = Func String [Term] | Var String deriving (Show, Eq)
type Subst = [(String, Term)]

unify :: Term -> Term -> Subst -> Maybe Subst
unify (Var x) y s | Just x' <- lookup x s = unify x' y s
unify x (Var y) s | Just y' <- lookup y s = unify x y' s

unify (Var x) (Var y) s | x == y = Just s
unify (Var x) y s = Just $ (x, y):s
unify x (Var y) s = Just $ (y, x):s
unify (Func x xs) (Func y ys) s | x == y && length xs == length ys = foldM f s (zip xs ys)
  where
    f :: Subst -> (Term, Term) -> Maybe Subst
    f s (a, b) = unify a b s
unify (Func _ _)  (Func _ _) _  | otherwise = Nothing

type State = (Subst, Int)
type Goal = State -> [State]  -- Computes all solutions

(===) :: Term -> Term -> Goal
(===) a b (s, v) = do
    s' <- maybeToList (unify a b s)
    return (s', v)

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
reify s (Var x') | Just y <- lookup x' s  = y
reify s x@(Var _) | otherwise = x
reify s (Func f xs) = Func f (map (reify s) xs)

solve :: Goal -> [Subst]
solve g = map fst $ g ([], 0)

isTrue :: Goal -> Bool
isTrue = not . null . solve

solutions :: Int -> (Term -> Goal) -> [Term]
solutions c g =
  let vtop = "_top" in
  let g' = g (Var vtop) in
  take c $ map (fromJust . lookup vtop) (solve g')

run :: (Term -> String) -> Int -> (Term -> Goal) -> IO ()
run p c g =
  mapM_ (putStrLn . p) (solutions c g)
