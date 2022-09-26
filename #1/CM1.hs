--CM1
--n^e recursively/recursivement
g :: (Eq t, Num t, Num p) => p -> t -> p
g n 0 = 1
g n e = n * g n (e - 1)

--pareil, mais plus rapide
--more efficient version
g' :: (Num a2, Integral a1) => a2 -> a1 -> a2
g' x 0 = 1
g' x n = if even n
    then g' (x*x) (div n 2)
    else x * g' (x*x) (div n 2)

--Fibonacci, version exponentielle
--Fibonacci, exponential version
fibBad :: Integer -> Integer
fibBad 0 = 0
fibBad 1 = 1
fibBad n = fibBad (n-1) + fibBad (n-2)

--Fibonacci, version linéaire
--Fibonacci, linear version
fib' :: Integer -> Integer -> Integer -> Integer
fib' 0 _ n2  = n2
fib' n n1 n2 = fib' (n-1) (n1 + n2) n1

fib :: Integer -> Integer
fib n = fib' n 1 0

--calculer l'hauteur palindromique
--calculating the "palindromic height"

--calule l'hauteur avec la fonction r que donne l'invers d'un nombre
--makes the calculation based on the function r that return the reversed number
hpal :: (Integral t, Num p) => t -> p
hpal n = if n == r n
    then 0
    else 1 + hpal (n + r n)

--donne l'invers d'un nombre
--returns the reversed numer
r1 :: Integral t => t -> t -> t
r1 0 m = m
r1 n m = r1 (div n 10) (10 * m + mod n 10)

r :: Integral t => t -> t
r x = r1 x 0;
