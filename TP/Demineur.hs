{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (transpose)
import System.Random ( newStdGen, Random(randomRs) )
import qualified Control.Monad.IO.Class

--Manque Q6, Q16, Q19
--Questions 6, 16, 19 need to be done

--Test final et jouer
--Do the final loops and play

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
randSet :: (Control.Monad.IO.Class.MonadIO m, Ord a, Ord b, Random a,
 Random b, Num a, Num b) =>
a -> b -> Int -> m (Set (a, b))
randSet haut larg n = do 
                    std1 <- newStdGen 
                    std2 <- newStdGen
                    return $ head (dropWhile ((\x -> length x /= n)) (scanl (\x y -> S.insert y x) (S.empty) (zip (randomRs (0, haut - 1) std1) (randomRs (0, larg - 1) std2))))

--Q6
--Create the grille based on randSet
--grid haut larg ensemble = 


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
--Update grid based on the new number of mines
--updateGrid :: p -> [[(Int, b)]] -> [[Cell -> Cell]]
--updateGrid cs xs = (map.map) (\(x, y) -> updateCell x) xs

--Q17
applyi :: (t -> t) -> Int -> [t] -> [t]
applyi f i xs = take i xs ++ [f (xs !! i)] ++ drop (i+1) xs

--Q18
applyij :: (t -> t) -> Int -> Int -> [[t]] -> [[t]]
applyij f i j xss = take i xss ++ [applyi f j (xss !! i)] ++ drop (i+1) xss


--Q19
--Uncover: Gives the grille obtained when exploring (clicking) one coordenate


--Q20
covIndic :: Num a => Cell -> a
covIndic (Covered x y z) = 1
covIndic Selected = 0
covIndic (Uncovered x) = 0


--Q21
won :: (Eq a, Num a) => [[Cell]] -> a -> Bool
won g n = foldl (+) 0 (map (sum . map covIndic) g) == n


--Q22
toggleFlag :: Cell -> Cell
toggleFlag (Covered x y z) = Covered x y (not z)



--Q23
--Loop1



--Q24
--Loop2



