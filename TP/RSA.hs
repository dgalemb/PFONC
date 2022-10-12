import Data.Char (ord, chr)
import Data.List (foldl')
import Data.List.Split (chunksOf)

--defining the main data type used and general fonctions, Q1
data Message = Message [Integer] deriving Show

--Q2
stringToMessage :: [Char] -> Message
stringToMessage xs = Message (map fromIntegral (map ord xs)) 

messageToString :: Message -> [Char]
messageToString (Message xs) = map chr (map fromIntegral xs)

--Q3
pad :: Int -> Message -> Message
pad longBloc (Message xs) =  
    let x = longBloc - (mod (length xs) longBloc)
    in 
        Message (xs ++ replicate x (fromIntegral x))

--Q4
unpad :: Message -> Message
unpad (Message xs) = 
    let x = fromIntegral (last xs)
    in 
        Message (take (length xs - x) xs)

--Q5
groupBytes :: Message -> Integer
groupBytes (Message (x:xs)) = foldl' ((+).(*256)) x xs

--Q6
ungroupBytes x y = snd (head (drop 3 (take (x + 1) (iterate (\(n, xs) -> (n `div` 256, (n `mod` 256):xs)) (y, [])))))

--Q7
groupN bsize (Message xs) = chunksOf bsize xs

--Q8
makeBlocks bsize (Message xs) = map groupBytes (map Message (groupN bsize (Message xs)))

--Q9
--splitBlocks bsize (Message xs) = 
--   let x = head xs
--    in
--    map helper x where 
--        helper xs = ungroupBytes bsize xs (++) splitBlocks bsize (Message (tail xs))



--Q10
ints m = (take m $ iterate (\x -> 6*x + 1) 1) ++ (tail (take m $ iterate (\x -> 6*x - 1) 1))

prime n = if n > 1 then null [x | x <- ints (round (sqrt (n)) `div` 6), mod n x (==) 0] else False

is_prime :: Int -> Bool
is_prime 1 = False
is_prime 2 = True
is_prime n | (length [x | x <- [2 .. round (n^(0.5))], mod n x == 0]) > 0 = False
		   | otherwise = True