import Data.Char (ord, chr)
import Data.List (foldl')
import Data.List.Split (chunksOf)

-- All done

--defining the main data type used and general fonctions, Q1
--data Message = Message [Integer] deriving Show

--Q2
--stringToMessage :: [Char] -> Message
--stringToMessage xs = Message (map (fromIntegral.ord) xs)

--messageToString :: Message -> [Char]
--messageToString (Message xs) = map (chr.fromIntegral) xs

--Q3
--pad :: Int -> Message -> Message
--pad longBloc (Message xs) =  
--   let x = longBloc - mod (length xs) longBloc
--    in 
--        Message (xs ++ replicate x (fromIntegral x))

--Q4
--unpad :: Message -> Message
--unpad (Message xs) = 
--    let x = fromIntegral (last xs)
--    in 
--        Message (take (length xs - x) xs)

--Q5
--groupBytes :: Message -> Integer
--groupBytes (Message (x:xs)) = foldl' ((+).(*256)) x xs

--Q6
--ungroupBytes :: Int -> Integer -> [Integer]
--ungroupBytes x y = snd (head (drop 3 (take (x + 1) (iterate (\(n, xs) -> (n `div` 256, (n `mod` 256):xs)) (y, [])))))

--Q7
--groupN1 bsize (Message xs) = chunksOf bsize xs

--Q8
--makeBlocks bsize (Message xs) = map (groupBytes . Message) (groupN1 bsize (Message xs))

--Q9



--Q10




-- Messages
data Message = Message [Integer]

stringToMessage :: String -> Message
stringToMessage s = Message $ map (fromIntegral.ord) s

messageToString :: Message -> String
messageToString (Message []) = []
messageToString (Message (x:xs)) = (chr.fromIntegral) x:messageToString (Message xs)

-- Blocs
ajoute :: Int -> Integer -> [Integer] -> [Integer]
ajoute 0 x l = l
ajoute n x l = ajoute (n-1) x (x:l)

pad :: Int -> Message -> Message
pad b (Message x) = 
    let n = length x; r = n `rem` b  in 
    if r == 0 
        then Message $ ajoute b (fromIntegral b) x
        else Message $ ajoute (b-r) (fromIntegral (b-r)) x

unpad :: Int -> Message -> Message
unpad b (Message x) = let x' = map fromIntegral x in Message $ map fromIntegral $ drop (head x') x'

groupBytes :: [Integer] -> Integer
groupBytes (x:xs) = foldl' ((+).(*256)) x xs

ungroupBytes :: Int -> Integer -> [Integer]
ungroupBytes bloc n = reverse $ map (`mod`256) $ take bloc $ iterate (`quot` 256) n

groupN :: Int -> [Integer] -> [[Integer]]
groupN n [] = []
groupN n l = let (pre,suf) = splitAt n l in pre: groupN n suf

makeBlocks :: Int -> Message -> Message
makeBlocks n (Message x) = Message $ map groupBytes (groupN n x)

splitBlocks :: Int -> Message -> Message
splitBlocks n (Message x) = Message $ concat $ map (ungroupBytes n) x

-- Chiffrement et déchiffrement
prime :: Integer -> Bool
prime n = not $ any ((== 0).(n `rem`)) [ a | a <- [2..((round.sqrt.fromIntegral) n)], a `rem` 6 == 5 || a `rem` 6 == 1 || a==2 || a==3]

choosePrime :: Integer -> Integer
choosePrime b = head $ dropWhile (not.prime) [b..]


euclid' :: Integer -> Integer -> Integer -> Integer-> Integer -> Integer -> [Integer] 
euclid' r u v 0 u' v' = [r,u,v]
euclid' r u v r' u' v' = euclid' r' u' v' (r-(r `quot` r')*r') (u-(r `quot` r')*u') (v-(r `quot` r')*v')

euclid :: Integer -> Integer -> [Integer]
euclid a b = euclid' a 1 0 b 0 1

modInv :: Integer -> Integer -> Integer
modInv e n = let d = head $ drop 1 $ euclid e n 
             in if d <0 then d + n 
                        else d 

modExp :: Integer -> Integer -> Integer -> Integer
modExp m 0 n = 1
modExp m e n = let modu = modExp m (e `quot` 2) n
               in if odd e then (modu*modu*(m `mod` n)) `mod` n
                           else (modu*modu) `mod` n

encrypt :: Integer -> Integer -> Int -> String -> Message
encrypt e n bsize str = let (Message x) = makeBlocks bsize (pad bsize (stringToMessage str))
                        in Message $ map (\a -> modExp a e n) x

decrypt :: Integer -> Integer -> Int -> Message -> String 
decrypt d n bsize (Message x) = let m2 = Message $ map (\c -> modExp c d n) x
                                in let m3 = unpad bsize $ splitBlocks bsize m2
                                   in  messageToString m3

main :: IO()
main = do        
    let p = choosePrime 10000000; q = choosePrime 7000000; n=p*q 
    let phi = (p-1)*(q-1); bsize=5
    let e = 65537; d = modInv e phi 
    let str = "coin coin disait le canard"
    let Message x = encrypt e n bsize str
    let s = decrypt d n bsize (Message x)
    print p
    print q
    print n
    print phi
    print e
    print d