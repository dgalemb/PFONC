--CM2
--ET logique
--logical AND
et::Bool->Bool->Bool
et True True = True
et _ _    = False

--points et Figures
--points and figures
data Point = Point Double Double deriving Show
data Vecteur = Vecteur Double Double deriving Show

data Figure = FigP Point | Cercle Point Double | Carre Point Vecteur

--fonction distance
--function distance
distance::Point->Point->Double
distance (Point x1 y1) (Point x2 y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

norme :: Vecteur -> Double
norme (Vecteur x y) = distance (Point 0 0) (Point x y)

--perimetre d'une figure
--perimeter of a figure
perimetre :: Figure -> Double
perimetre (FigP _) = 0
perimetre (Cercle _ r) = 2 * pi * r
perimetre (Carre _ v) = 4 * norme v



--types algebriques récursifs
--recursive algebraic types
data Nat = Zero | Succ Nat deriving Show

--profondeur
--depth
eval :: Nat-> Integer
eval Zero = 0
eval (Succ n) = 1 + eval n

--somme
--sum
addition :: Nat -> Nat -> Nat
addition Zero n = n
addition (Succ n1) n2 = addition n1 (Succ n2)