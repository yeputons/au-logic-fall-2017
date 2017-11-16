module Lambda(Prog(Ref, (:=>), (:@:)), compute) where

data Prog =
    Ref String
  | String :=> Prog
  | Prog :@: Prog
  deriving (Eq, Show)

infixr 5 :=> 
infixl 6 :@:

type State = [(String, Prog)]

compute' :: State -> Prog -> Prog
compute' s (Ref x) | Just x' <- lookup x s = compute' s x'
compute' _ (Ref x) | otherwise             = Ref x
compute' _ x@(_ :=> _) = x
compute' s (p :@: q) =
  case compute' s p of
    (x :=> p') -> compute' ((x, q):s) p'
    p'         -> (p' :@: (compute' s q))

compute :: Prog -> Prog
compute = compute' []
