module Task01 (Point, getOffset, getMinPath, solve) where

type Point = (Int, Int)

(+++) :: Point -> Point -> Point
(x1, y1) +++ (x2, y2) = (x1 + x2, y1 + y2)

getOffset :: String -> Point
getOffset [] = (0, 0)
getOffset ('N':s) = ( 0,   1) +++ (getOffset s)
getOffset ('S':s) = ( 0, (-1)) +++ (getOffset s)
getOffset ('W':s) = ((-1),  0) +++ (getOffset s)
getOffset ('E':s) = (  1,  0) +++ (getOffset s)

getMinPath :: Point -> String
getMinPath (0, 0) = ""
getMinPath (0, y) | y > 0 = 'N':getMinPath (0, y - 1)
getMinPath (0, y) | y < 0 = 'S':getMinPath (0, y + 1)
getMinPath (x, y) | x < 0 = 'W':getMinPath (x + 1, y)
getMinPath (x, y) | x > 0 = 'E':getMinPath (x - 1, y)

solve :: String -> String
solve = getMinPath . getOffset

main = interact (unlines . map solve . lines)
