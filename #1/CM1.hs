--CM1
--n^e recursively/recursivement
g n 0 = 1;
g n e = n * g n (e - 1);

--calculer l'hauteur palindromique
--calculating the "palindromic height"

--calule l'hauteur avec la fonction r que donne l'invers d'un nombre
--makes the calculation based on the function r that return the reversed number
hpal n = if n == r n
    then 0
    else 1 + hpal (n + r n);

--donne l'invers d'un nombre
--returns the reversed numer
r1 0 m = m;
r1 n m = r1 (div n 10) (10 * m + mod n 10);
r x = r1 x 0;

--c = 1243;
--main :: IO ();
--main = do hpal 123;