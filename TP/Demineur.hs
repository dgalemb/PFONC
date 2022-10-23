import Data.Set (Set)
import Data.List (transpose)
import System.Random
import qualified Data.Set as S

--Q1
-- Mine, Drap
data Cell = Covered Int Bool Bool 
            | Uncovered Int 
            | Selected

--Q2
data Grid = Grid [[Cell]]

--Q3
instance Show Cell where
    show :: Cell -> String
    show Selected = show '.'
    show (Uncovered n) = show '.'
    show (Covered n _ True) = show 'P'
    show (Covered n _ False) = show 'o'

--Q4
instance Show Grid where
    show :: Grid -> String
    show (Grid x) = unlines $ map (concatMap show) x

--Q5
randSet :: (Ord a, Ord b, Random a, Random b, RandomGen g1, RandomGen g2,
 Num a, Num b) => p -> g1 -> g2 -> a -> b -> Set [(a, b)]
randSet n std1 std2 lig col = let ss = S.empty
    in S.insert (take 1 (zip (randomRs (0, lig) std1) (randomRs (0, col) std2))) ss

--Q6


--Q7
mineIndic :: Num a => Cell -> a
mineIndic (Covered x y z) = if y == True
                 then 1
                 else 0
mineIndic (Uncovered x) = 0
mineIndic (Selected) = 0

--Q8
mines :: Num b => Grid -> [[b]]
mines (Grid x) = map (map mineIndic) x

--Q9
moveUp :: Num a => [[a]] -> [[a]]
moveUp l = tail l ++ [replicate (length (head l)) 0]

--Q10
moveDown :: Num a => [[a]] -> [[a]]
moveDown l = [replicate (length (head l)) 0] ++ init l

--Q11
moveRight :: Num a => [[a]] -> [[a]]
moveRight l = transpose (moveDown (transpose l))
moveLeft :: Num a => [[a]] -> [[a]]
moveLeft l = transpose (moveUp (transpose l))

--Q12
gridMoves :: Num a => [[a]] -> [[[a]]]
gridMoves l = [moveDown l, moveUp l] ++ map moveRight [l, moveDown l, moveUp l] ++ map moveLeft [l, moveDown l, moveUp l]

--Q13
matrixSum :: Num c => [[c]] -> [[c]] -> [[c]]
matrixSum _ [] = []
matrixSum [] _ = []
matrixSum x y = [zipWith (+) (head x) (head y)] ++ matrixSum (tail x) (tail y)

--Q14
neighbourMap :: Num c => [[c]] -> [[c]]
neighbourMap l = let x = gridMoves l
                 in matrixSum (matrixSum (matrixSum (head x) (x !! 1)) (matrixSum (x !! 2) (x !! 3)))
                 (matrixSum (matrixSum (x !! 4) (x !! 5)) (matrixSum (x !! 6) (x !! 7)))

--Q15
updateCell :: Int -> Cell -> Cell
updateCell n (Covered k x y) = Covered n x y
updateCell n (Uncovered k) = Uncovered n
updateCell n Selected = Selected

--Q16
--updateGrid :: p -> [[(Int, b)]] -> [[Cell -> Cell]]
--updateGrid cs xs = (map.map) (\(x, y) -> updateCell x) xs

--Q17
applyi f i xs = take i xs ++ [f (xs !! i)] ++ drop (i+1) xs

--Q18
applyij f i j xss = take i xss ++ [applyi f j (xss !! i)] ++ drop (i+1) xss


--Q19



--Q20
covIndic (Covered x y z) = 1
covIndic Selected = 0
covIndic (Uncovered x) = 0


--Q21
won g n = foldl (+) 0 (map (sum . map covIndic) g) == n


--Q22
toggleFlag (Covered x y z) = Covered x y (not z)