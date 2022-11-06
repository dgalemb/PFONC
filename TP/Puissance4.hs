--Il manque presque tout
--Almost everything needs to be done

--Q1
data Color = Rouge | Orange

--Q2
data Cell = Vide | Joueur Color

--Q3
data Col = Col [Cell]
type Grid = Grid [Col]

--Q4
start = replicate 7 (Column (replicate 6 Cell))

--Q5
instance Show Cell where
    show Vide = show 'O'
    show (Joueur Rouge) = show 'X'
    show (Joueur Orange) = show 'P'

instance Show Grid where
        show (Grid x) = unlines $ (map) (concatMap show) x