--CM4

--anagrammes
--calculating all anagrams for one word

--import Data.List (delete)

an :: String -> [String];
an "" = [""];
an xs = map (\x -> map (x:) $ an $ delete x xs) $ nub xs;

--recursion sur les listes
--recursion in lists

--avec/with filter

diviseurs n = filter ((==0).(rem n)) [1..n];
premiers n = filter ((==2).length.(diviseurs)) [2..n-1];

--version plus efficace
--bettered version
divisors1 n = filter ((==0).(rem n)) $ takeWhile ((<=n).(^2)) primes;
primes1 n = 2:filter (null.divisors) [3..];


--avec/with foldl ou foldr

--somme
--sum
sum1 :: [Integer]->Integer;
sum1 = foldl (+) 0;

--max
--max
max1 :: [Integer]->Integer;
max1 [] = error "Maximum : liste vide";
max1 = foldl (max);

--ET logique
--logical AND
and1 :: [Bool] -> Bool;
and1 = foldr (&&) True;

--OR logique
--logical OR
or1 :: [Bool] -> Bool;
or1 = foldr (||) False;

--concatenation
--concatenating
concat1 :: [[a]] -> [a]
concat1 = foldl (++) [];

