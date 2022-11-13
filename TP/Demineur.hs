{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (transpose)
import System.Random ( newStdGen, Random(randomRs) )
import qualified Control.Monad.IO.Class
import System.IO
-- import System.Random (StdGen, mkStdGen, next)
import Data.Set (Set)
import Data.List (transpose)
--Windows bug Fix
import Data.Char
import Control.Monad(liftM)
import Foreign.C.Types
getHiddenChar = liftM (chr. fromEnum ) c_getch
foreign import ccall unsafe "conio.h getch"
    c_getch :: IO CInt

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
    show (Uncovered n) = show n
    show (Covered n _ True) = show 'P'
    show (Covered n _ False) = show 'o'

--Q4
instance Show Grid where
    show :: Grid -> String
    show (Grid x) = unlines $ map (concatMap show) x


--Q5

--randSet haut larg n = do 
--                   std1 <- newStdGen 
--                    std2 <- newStdGen
--                    return $ head (dropWhile ((\x -> length x /= n)) (scanl (\x y -> S.insert y x) (S.empty) (zip (randomRs (0, haut - 1) std1) (randomRs (0, larg - 1) std2))))

--Q6
grid :: Int -> Int -> [(Int,Int)] -> Grid
grid h l m = Grid ([[ if (x, y) `elem` m then Covered 0 True False else Covered 0 False False | x <- [0..l-1]] | y <- [0..h-1]])


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
updateGrid :: Grid -> [[Int]] -> Grid
updateGrid (Grid m) l = Grid (zipWith (\rC rN -> zipWith (\c n -> updateCell c n) rC rN) m l)

--Q17
applyi :: (t -> t) -> Int -> [t] -> [t]
applyi f i xs = take i xs ++ [f (xs !! i)] ++ drop (i+1) xs

--Q18
applyij :: (t -> t) -> Int -> Int -> [[t]] -> [[t]]
applyij f i j xss = take i xss ++ [applyi f j (xss !! i)] ++ drop (i+1) xss


--Q19
uncover :: Int -> Int -> Grid -> Grid
uncover i j (Grid m) = Grid (applyij (\(Covered a p b) -> Uncovered a) i j m)

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
loop :: Int -> Int -> Int -> Grid -> IO ()
loop i j n b@( Grid xs) -- le paramètre b se décompose en (Grid xs)
    | won n b = putStrLn " Victoire  !"
    | otherwise = do
        -- affiche la grille avec la case i, j sélectionnée
        putStrLn $ show $ Grid $ applyij ( const Selected ) i j xs
        -- lit un caractère
        c <- getHiddenChar
        case c of
            'i' -> loop (max (i - 1) 0) j n b -- bouge le curseur vers le haut
            'k' -> loop (min (i + 1) (length xs)) j n b -- bouge le curseur vers le bas
            'j' -> loop i (max (j - 1) 0) n b -- bouge le curseur vers la gauche
            'l' -> loop i (min (j + 1) (length (head xs))) n b -- bouge le curseur vers la droite
            'f' -> loop i j n (Grid $ applyij toggleFlag i j xs) -- pose ou enlève un drapeau sur la case i, j
            'u' -> if mineIndic (xs!!i!!j)==1
                then putStrLn "Failure" --TODO putStrLn $ show (mineIndic (xs!!i!!j))
                else loop i j n (uncover i j b)
             -- découvre la case i, j; BOUM ?
            otherwise -> loop i j n b -- ne fait rien



--Q24
main :: IO ()
main = do
    -- désactive l’attente de la touche entrée pour l’acquisition
    hSetBuffering stdin NoBuffering
    -- désactive l’écho du caractère entré sur le terminal
    hSetEcho stdin False
    -- récupère deux StdGen pour la génération aléatoire
    --g <- newStdGen
    --g' <- newStdGen
    -- nombre de mines, lignes, colonnes
    let nmines = 2
    let l = 7
    let c = 10
    -- creer la grille, ajouter les mines, mettre a jour les voisins
    --let mines = randSet nmines g g'
    let g = grid l c [(0,0), (0,1)]
    let gMines = neighbourMap (mines g)
    loop 2 2 nmines (updateGrid g gMines); -- démarrer la REPL


