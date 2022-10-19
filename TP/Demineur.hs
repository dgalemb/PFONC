import Data.Set (Set)
import Data.List (transpose)
import System.Random
import qualified Data.Set as S

--Q1
data Cell = Covered Int Bool Bool 
            | Uncovered Int 
            | Selected

--Q2
data Grid = Grid [[Cell]]

--Q3
instance Show Cell where
    show Selected = show '.'
    show (Uncovered n) = show '.'
    show (Covered n _ True) = show 'P'
    show (Covered n _ False) = show 'o'

--Q4
instance Show Grid where
    show (Grid x) = unlines $ map (concatMap show) x

--Q5
randSet n std1 std2 lig col = let ss = S.empty
    in S.insert (take 1 (zip (randomRs (0, lig) std1) (randomRs (0, col) std2))) ss

--Q6


--Q7
mineIndic (Covered x y z) = if y == True
                 then 1
                 else 0
mineIndic (Uncovered x) = 0
mineIndic (Selected) = 0

--Q8
mines (Grid x) = map (map mineIndic) x

--Q9
moveUp l = tail l ++ [replicate (length (head l)) 0]

--Q10
moveDown l = [replicate (length (head l)) 0] ++ init l

--Q11
moveRight l = transpose (moveDown (transpose l))
moveLeft l = transpose (moveUp (transpose l))

--Q12
gridMoves l = [moveDown l, moveUp l] ++ map moveRight [l, moveDown l, moveUp l] ++ map moveLeft [l, moveDown l, moveUp l]

--Q13
matrixSum _ [] = []
matrixSum [] _ = []
matrixSum x y = [zipWith (+) (head x) (head y)] ++ matrixSum (tail x) (tail y)

--Q14
neighbourMap l = let x = gridMoves l
                 in matrixSum (matrixSum (matrixSum (head x) (x !! 1)) (matrixSum (x !! 2) (x !! 3)))
                 (matrixSum (matrixSum (x !! 4) (x !! 5)) (matrixSum (x !! 6) (x !! 7)))

--Q15
updateCell Cell n = 