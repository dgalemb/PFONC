--CM3
--definition type liste (custom)
--defining our custom data type list (of integers)
data Liste = Vide | Cons Integer Liste

--somme des éléments
--sum of the list elements
somme :: Liste -> Integer
somme Vide = 0
somme (Cons x xs) = x + somme xs

--inversion liste
--reversing a list
inver :: [a] -> [a]
inver [] = []
inver (x:xs) = inver xs ++ [x]

inver' :: [a] -> [a] -> [a]
inver' [] rs = rs
inver' (x:xs) rs = inver' xs (x:rs)

rev :: [a] -> [a]
rev xs = inver' xs []

--effacer premier occurrence d'un element
--deleting the first copy of an element in a list
suppr :: Eq t => t -> [t] -> [t]
suppr x [] = [];
suppr x (y:ys) = if x == y then ys else y:suppr x ys;

--Max en la liste
--Returning the biggest number in a list
maxi :: Ord a => [a] -> a
maxi  [] = error "maxi: liste vide"
maxi [x] = x;
maxi (x:xs) = max x (maxi xs);

--Trimax
--Similarly
trimax :: Ord a => [a] -> [a]
trimax [] = [];
trimax xs = let y = maxi xs
            in y: trimax (suppr y xs);

--definition arbre binaire
--definig a binary tree
data ArbreB = VideA | ConsA Integer ArbreB ArbreB

--hauteur de l'arbre
--height of the tree
hauteur :: (Num p, Ord p) => ArbreB -> p
hauteur VideA = 0
hauteur (ConsA _ xs ys) = 1 + max (hauteur xs) (hauteur ys)


--liste comprehension: Triples pythagoriciens tq a + b + c = 1000
--calculating the pythagorian triples that obey a + b + c = 1000
triplet :: [(Integer, Integer, Integer)]
triplet = [(a, b, c) |
                   a <- [1,2..333],
                   b <- [a+1..500],
                   c <- [b+1..500],
                   a*a + b*b == c*c,
                   a + b + c == 1000];


--Fermeture
--Closure
k :: Num a => a -> a -> a
k y = (\x -> x + y);