--CM3
--definition type liste (custom)
--defining our custom data type list (of integers)
data Liste = Vide | Cons Integer Liste;

--somme des éléments
--sum of the list elements
somme Vide = 0;
somme (Cons x xs) = x + somme xs;

--definition arbre binaire
--definig a binary tree
data ArbreB = VideA | ConsA Integer ArbreB ArbreB;

--hauteur de l'arbre
--height of the tree
hauteur VideA = 0;
hauteur (ConsA _ xs ys) = 1 + max (hauteur xs) (hauteur ys);

--liste comprehension: Triples pythagoriciens tq a + b + c = 1000
--calculating the pythagorian triples that obey a + b + c = 1000
triplet = [(a, b, c) |
                   a <- [1,2..333],
                   b <- [a+1..500],
                   c <- [b+1..500],
                   a*a + b*b == c*c,
                   a + b + c == 1000];

--inversion liste
--reversing a list
inver [] = [];
inver (x:xs) = inver xs ++ [x];

--effacer premier occurrence d'un element
--deleting the first copy of an element in a list
suppr x [] = [];
suppr x (y:ys) = if x == y then ys else y:suppr x ys;

--Max en la liste
--Returning the biggest number in a list
maxi  [] = error "maxi: liste vide"
maxi [x] = x;
maxi (x:xs) = max x (maxi xs);

--Trimax
--Similarly
trimax [] = [];
trimax xs = let y = maxi xs
            in y: trimax (suppr y xs);

--Fermeture
--Closure
k y = (\x -> x + y);