import Data.Char (ord, chr)


--Q1.1
data Entier = Poss Int Int    
            | NotPoss Int

valeur (NotPoss 1) = 1
valeur (NotPoss 2) = 2
valeur (NotPoss 5) = 5
valeur (Poss a b) = 3*a + 4*b 

--Q1.2
data Point = Cart Float Float 
            | Pol Float Float

dOrigine (Cart x y) = sqrt (x*x + y*y)
dOrigine (Pol x y) = x

--Q1.3
data ExprA = Num Int
            | Oper String ExprA ExprA

evaluate :: ExprA -> Int
evaluate (Num a) = a
evaluate (Oper x y z) = if x == "*" then evaluate y * evaluate z else evaluate y + evaluate z

--Q2.1
euclide a b = if a `mod` b == 0 then minimum(a, b) else euclide b (a `mod` b)

--Q2.2
somme 0 = 0
somme n = n + somme (n-1)


somme' 0 r = r
somme' n r = somme' (n-1) (r + n)
somme'' n = somme' n 0


parfait n = let a = [b |
                          b <- [1..n-1],
                          n `mod` b == 0]
              in (n == sum a)

--Q2.3

syracuse :: Int -> Int -> Int
syracuse x 0 = x
syracuse x n = if even x then syracuse (div x 2) (n-1) else syracuse (3*x + 1) (n-1)

--Q2.4
f' :: [Char] -> Integer -> Integer -> Bool
f' [] n m = if n /= m then False else True
f' (x:xs) n m 
    | m > n = False
    | x == '(' = f' xs (n+1) m
    | x == ')' = f' xs n (m+1)

f :: [Char] -> Bool
f (x:xs) = f' (x:xs) 0 0

--Q2.5


--Q2.6

puissancef f a b = last $ take (a + 1) $ iterate f b 

summ :: Int -> Int
summ 0 = 0
summ n = n + summ (n-1)

--Q2.7
ff :: Integral t => [a] -> t -> [a]
ff [] _ = []
ff (x:xs) i = if i `mod` 2 == 0 then [x] ++ ff xs (i+1) else ff xs (i+1)

rangsPaires :: [a] -> [a]
rangsPaires (x:xs) = ff (x:xs) 0

rangsPaires' xs = map snd $ filter (\x -> even (fst x)) (zip [0..length xs - 1] xs)

--Q2.8
drop' n xs = map snd $ filter (\x -> (fst x) > n) (zip [1..length xs] xs)

takex 0 xs r = r
takex n xs r = if n < length xs then takex (n-1) (tail xs) (r ++ [head xs]) else xs

take' n xs = takex n xs []

splitAt' n xs = (take' n xs, drop' n xs)

--Q3.1
map2D f xxs = (map (map f)) xxs


--Q3.2
petitePuissance x y = let lis = iterate (*x) 1
                    in (map fst $ take 1 $ filter (\x -> snd x == True) $ zip (lis) (map (\x -> x > y) lis)) !! 0


--Q3.3
cesar :: Int -> String -> String
cesar n = let a = ord 'a'
        in map (chr.(+a).(`mod` 26).(+n).(subtract a).ord)

execCesar :: IO ()
execCesar = do
    s <- getLine
    n <- getLine
    putStrLn $ cesar (read n) s

--Q4.1

--Q4.2
sommePairs :: [Int] -> Int
sommePairs xs = sum.filter even $ xs

--Q5.1
reverse' xs = foldl (\acc x-> x : acc) [1,4,5,6] xs

--Q5.2
evalPoly ks x = foldl (+) 0 (zipWith (*) ks (take (length ks) $ iterate (*x) 1))

--Q5.3

--Q6.1
argminf :: (Num a1, Enum a1, Ord a2) => (a1 -> a2) -> a2 -> [a1]
argminf f x = take 1 $ [a |
                a <- [1..],
                f a > x]

--Q6.2


main = do   
    line <- getLine  
    let enil = reverse line 
        in putStrLn (line ++ " " ++ enil)
  