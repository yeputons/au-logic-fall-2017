module Task3 where
import Data.List
import Minikanren
import Lists

fox = Func "fox" []
goose = Func "goose" []
beans = Func "beans" []
empty = Func "empty" []

left = Func "left" []
right = Func "right" []

goodSide :: Term -> Goal
goodSide s = (notElemo fox s ||| notElemo goose s) &&& (notElemo goose s ||| notElemo beans s)

state :: Term -> Term -> Term -> Term
state l r boatPos = Func "state" [l, r, boatPos]

goodState :: Term -> Goal
goodState s = fresh $ \l -> fresh $ \r -> fresh $ \boatPos -> (state l r boatPos) === s &&& (boatPos === left &&& goodSide r ||| boatPos === right &&& goodSide l)

finishState :: Term -> Goal
finishState s = fresh $ \l -> fresh $ \r -> fresh $ \boatPos -> (state l r boatPos) === s &&& l === nil &&& boatPos === right

removeo :: Term -> Term -> Term -> Goal
removeo l x l' =
   (l === nil &&& l' === nil) |||
   (fresh $ \h -> fresh $ \t -> l === h-:-t &&& (
     (h === x &&& l' === t) |||
     (h =/= x &&& (fresh $ \t' -> l' === h-:-t' &&& removeo t x t'))
   ))

canTransfer :: Term -> Term -> Term -> Term -> Term -> Goal
canTransfer l1 r1 l2 r2 x =
  x === empty &&& l1 === l2 &&& r1 === r2 |||
  (x =/= empty &&& (
   fresh $ \t -> r2 ===  x -:- r1 &&& removeo l1 x l2
  ))

canMove :: Term -> Term -> Term -> Goal
canMove from to x =
  fresh $ \l1 -> fresh $ \r1 -> fresh $ \s1 -> from === state l1 r1 s1 &&& (
  fresh $ \l2 -> fresh $ \r2 -> fresh $ \s2 -> to === state l2 r2 s2 &&& (
    (s1 === left &&& s2 === right &&& canTransfer l1 r1 l2 r2 x) |||
    (s1 === right &&& s2 === left &&& canTransfer r1 l1 r2 l2 x)
  )
  )
  &&& goodState from &&& goodState to

canMoveSeveral :: Term -> Term -> Term -> Goal
canMoveSeveral from to xs =
  (xs === nil &&& from === to) ||| (
  fresh $ \h -> fresh $ \t -> xs === h -:- t &&& (
    fresh $ \q -> canMove from q h &&& canMoveSeveral q to t
  )
  )

showStr :: Term -> String
showStr (Func x []) = x

showStrs :: Term -> String
showStrs s = "[" ++ (concat $ intersperse ", " (map showStr $ listToHlist s)) ++ "]"

showState :: Term -> String
showState (Func "state" [l, r, Func side []]) = "(" ++ showStrs l ++ ", " ++ showStrs r ++ ", " ++ side ++ ")"

showCanMoveSeveralResult :: PSol -> String
showCanMoveSeveralResult (PSol e []) =
  let Just initial = lookup "initial" e in
  let Just goal = lookup "goal" e in
  let Just cmds = lookup "cmds" e in
  showState initial ++ "  ---> " ++ showState goal ++ " via " ++ showStrs cmds
showCanMoveSeveralResult (PSol e _)  = "<disequality-constraints-detected>"

initial :: Term
initial = state (hlistToList [fox, goose, beans]) (hlistToList []) left

igoal :: Term
igoal = state (hlistToList []) (hlistToList [goose, beans, fox]) right

main :: IO ()
main = do
    --mapM_ (putStrLn . showCanMoveSeveralResult) $ solve (Var "initial" === initial &&& canMoveSeveral (Var "initial") (Var "goal") (Var "cmds") &&& Var "goal" === igoal)
    --run showStrs 1 $ canMoveSeveral initial igoal
    run showStrs 1 $ \s -> canMoveSeveral initial (Var "goal") s &&& finishState (Var "goal")
