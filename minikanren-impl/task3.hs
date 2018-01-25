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
goodState s = fresh $ \l -> fresh $ \r -> fresh $ \boatPos -> (state l r boatPos) === s &&& goodSide l &&& goodSide r

finishState :: Term -> Goal
finishState s = fresh $ \l -> fresh $ \r -> fresh $ \boatPos -> (state l r boatPos) === s &&& l === nil &&& boatPos === right

canTransfer :: Term -> Term -> Term -> Term -> Term -> Goal
canTransfer l1 r1 l2 r2 x =
  (x === empty &&& l1 === l2 &&& r1 === r2) ||| (
    fresh $ \h -> fresh $ \t -> l1 === h -:- t &&& (
    (x === h &&& l2 === t &&& r2 === (h -:- r1)) ||| (
      fresh $ \t' -> l2 === h -:- t' &&&
        canTransfer t r1 t' r2 x
    )
    )
  )

canMove :: Term -> Term -> Term -> Goal
canMove from to x =
  fresh $ \l1 -> fresh $ \r1 -> fresh $ \s1 -> from === state l1 r1 s1 &&& (
  fresh $ \l2 -> fresh $ \r2 -> fresh $ \s2 -> to === state l2 r2 s2 &&& (
    (s1 === left &&& s2 === right &&& canTransfer l1 r1 l2 r2 x) |||
    (s1 === right &&& s2 === left &&& canTransfer l2 r2 l1 r1 x)
  )
  )

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

main :: IO ()
main = do
    run showCommands 10 $ canMoveSeveral initial $ state (hlistToList [fox, goose, beans]) (hlistToList []) right
