--CM4
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use concatMap" #-}

import Data.List (delete, nub, foldl1)

--anagrammes
--calculating all anagrams for one word
an :: String -> [String]
an "" = [""]
an xs = concat $ map (\x -> map (x:) $ an $ delete x xs) $ nub xs


--recursion sur les listes
--recursion in lists

--avec/with filter
divisors :: Integral a => a -> [a]
divisors n = filter ((==0).rem n) [1..n]

primes :: Integral a => a -> [a]
primes n = filter ((==2).length.divisors) [2..n-1]

--version plus efficace
--bettered version
divisors1 :: Integer -> [Integer]
divisors1 n = filter ((==0).rem n) $ takeWhile ((<=n).(^2)) primes1

primes1 :: [Integer]
primes1 = 2:filter (null.divisors) [3..]

--foldS

--somme
--sum
sum1 :: [Integer]->Integer
sum1 = foldl (+) 0

--max
--max
max1 :: Ord p => [p] -> p
max1 [] = error "Maximum : liste vide"
max1 xs = foldl1 max xs

--ET logique
--logical AND
and1 :: [Bool] -> Bool
and1 = foldr (&&) True

--OR logique
--logical OR
or1 :: [Bool] -> Bool
or1 = foldr (||) False

--concatenation
--concatenating
concat1 :: [[a]] -> [a]
concat1 = foldl (++) []

--any
any1 :: (a -> Bool) -> [a] -> Bool
any1 f = or1.(map f)