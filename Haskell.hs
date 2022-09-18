--CM1
--n^e recursively:
g n 0 = 1;
g n e = n * g n (e - 1);

main1 = print (g 5 3);

--calculer l'hauteur palindromique

--calule l'hauteur avec la fonction r que donne l'invers d'un nombre
hpal n = if n == r n
    then 0
    else 1 + hpal (n + r n)

--donne l'invers d'un nombre
r1 0 m = m
r1 n m = r1 (div n 10) (10 * m + mod n 10)
r x = r1 x 0

main2 = print (hpal 1221)


--CM2
--ET logique

et::Bool->Bool->Bool;
et x y = if x == False || y == False then False else True;

main3 = print (et True True)

--Points et Figures
data Point = Point Double Double;
data Vecteur = Vecteur Double Double;


data Figure = FigP Point | Cercle Point Double | Carre Point Vecteur;

--Fonction distance
--distance::Point->Point->Point;
distance (Point x1 y1) (Point x2 y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2);
norme (Vecteur x y) = distance (Point 0 0) (Point x y)

--perimetre d'une figure
perimetre (FigP _) = 0;
perimetre (Cercle _ r) = 2 * pi * r;
perimetre (Carre _ v) = 4 * norme v;

--types algebriques récursifs
data Nat = Zero | Succ Nat;

eval :: Nat-> Integer;

eval Zero = 0;
eval (Succ n) = 1 + eval n;

addition Zero n = n;
addition (Succ n1) n2 = addition n1 (Succ n2);


--CM3
--Definition type liste (custom)
data Liste = Vide | Cons Integer Liste;

--Somme
somme Vide = 0;
somme (Cons x xs) = x + somme xs;

--Definition arbre binaire
data ArbreB = VideA | ConsA Integer ArbreB ArbreB;

--Hauteur
hauteur VideA = 0;
hauteur (ConsA _ xs ys) = 1 + max (hauteur xs) (hauteur ys);

--Liste comprehension: Triples pythagoriciens tq a + b + c = 1000
triplet = [(a, b, c) |
                   a <- [1,2..333],
                   b <- [a+1..500],
                   c <- [b+1..500],
                   a*a + b*b == c*c,
                   a + b + c == 1000];

--Inversion liste
inver [] = [];
inver (x:xs) = inver xs ++ [x];

--Effacer premier occurrence d'un element
suppr x [] = [];
suppr x (y:ys) = if x == y then ys else y:suppr x ys;

--Max en la liste
maxi  [] = error "maxi: liste vide"
maxi [x] = x;
maxi (x:xs) = max x (maxi xs);

--Trimax
trimax [] = [];
trimax xs = let y = maxi xs
            in y: trimax (suppr y xs);

--Closure

k y = (\x -> x + y);

--CM4

--anagrammes

--import Data.List (delete)

an :: String -> [String];
an "" = [""];
an xs = map (\x -> map (x:) $ an $ delete x xs) $ nub xs;

--recursion sur les listes

diviseurs n = filter ((==0).(rem n)) [1..n];
premiers n = filter ((==2).length.(diviseurs)) [2..n-1];


--foldS

-- Sum
sum1 :: [Integer]->Integer;
sum1 = foldl (+) 0;

-- Max
max1 :: [Integer]->Integer;
max1 [] = error "Maximum : liste vide";
max1 = foldl (max);

-- And
and1 :: [Bool] -> Bool;
and1 = foldr (&&) True;

or1 :: [Bool] -> Bool;
or1 = foldr (||) False;

-- Concat
concat1 :: [[a]] -> [a]
concat1 = foldl (++) [];

-- Any
any1 f = or1.(map f)

divisors n = filter ((==0).(rem n)) $ takeWhile ((<=n).(^2)) primes;
primes n = 2:filter (null.divisors) [3..];