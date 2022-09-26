--CM5
--entrees, Sorties en Haskell
--inputs and Outputs in Haskell


import Data.Char (ord, chr)
import Data.Maybe  (isNothing, fromJust)

--fonction pour faire l'inver d'un mot
--function that returns the reversed word
inver [] = []
inver (x:xs) = inver xs ++ [x]

--on prend l'entree
--getting input
echo = getLine >>= putStrLn 

--la sortie est l'entree plus l'entree inversé
--output = input + tupni
echo1 = getLine >>= (\x -> putStrLn (x ++ " " ++ inver x))

--la sortie est les deux lignes reçus en ordre inversée
--output is the two lines of input in reversed order
echo2 = getLine >>= (\x -> getLine >>= (\y -> putStrLn (y ++ " " ++ x)))

--Longueur de l'entree
--Lenght of input
ioLenght :: IO Int
ioLenght = getLine >>= return.length

--code de cesar
--returns the cesar code of the input
cesar :: Int -> String -> String
cesar n = let a = ord 'a'
        in map (chr.(+a).(`mod` 26).(+n).(subtract a).ord)

execCesar :: IO ()
execCesar = do
    s <- getLine
    n <- getLine
    putStrLn $ cesar (read n) s

loopCesar :: IO ()
loopCesar = do
    n <- getLine
    let x = read n :: Int
    if x >= 1 && x <= 25 
        then do 
            s <- getLine
            putStrLn $ cesar x s
        else 
            putStrLn "try again" >> loopCesar



--foncteurs
data Expr a = Val a 
            | Inc (Expr a) 
            | Dec (Expr a) 
            | Inv (Expr a) 
            | Neg (Expr a)
            | Add (Expr a) (Expr a)
            | Sub (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)
            | Div (Expr a) (Expr a)
            | Add3 (Expr a) (Expr a) (Expr a)
            deriving Show

evaluate :: (Fractional a) => Expr a -> a
evaluate (Val x) = x
evaluate (Inc y) = evaluate y + 1
evaluate (Dec y) = evaluate y - 1
evaluate (Neg y) = negate $ evaluate y
evaluate (Inv y) = 1 / evaluate y
evaluate (Add y z) = evaluate y + evaluate z
evaluate (Sub y z) = evaluate y - evaluate z
evaluate (Mul y z) = evaluate y * evaluate z
evaluate (Div y z) = evaluate y / evaluate z
evaluate (Add3 x y z) = evaluate x + evaluate y + evaluate z

isZero :: (Eq a, Num a) => Maybe a -> Bool
isZero Nothing = False
isZero (Just x) = (x == 0)

--mauvaise choix de comment le faire
--inefficient way to do it

--mevaluate' :: (Eq a, Fractional a) => Expr a -> Maybe a
--mevaluate' (Val x) = Just x
--mevaluate' (Inc y) = let my = mevaluate' y
--                     in if isNothing my 
--                           then Nothing
--                           else Just $ 1 + fromJust my
--mevaluate' (Dec y) = let my = mevaluate' y
--                     in if isNothing my 
--                           then Nothing
--                           else Just $ fromJust my - 1
--mevaluate' (Neg y) = let my = mevaluate' y
--                     in if isNothing my 
--                           then Nothing
--                           else Just $ negate $ fromJust my
--mevaluate' (Inv y) = let my = mevaluate' y
--                     in if isNothing my || isZero my
--                           then Nothing
--                           else Just $ 1 / fromJust my

mfmap :: (a -> a) -> Maybe a -> Maybe a
mfmap _ Nothing  = Nothing
mfmap f (Just x) = Just $ f x

mlift2' :: (a -> b -> c) -> Maybe a -> Maybe b->Maybe c
mlift2' _ Nothing _ = Nothing
mlift2' _ _ Nothing = Nothing
mlift2' f (Just x) (Just y) = Just $ f x y


mevaluate :: (Eq a, Fractional a) => Expr a -> Maybe a
mevaluate (Val x) = Just x
mevaluate (Inc y) = mfmap (+1) $ mevaluate y
mevaluate (Dec y) = mfmap (subtract 1) $ mevaluate y
mevaluate (Neg y) = mfmap negate $ mevaluate y
mevaluate (Inv y) = let my = mevaluate y
                    in if isZero my then Nothing
                                     else mfmap (1/) my
mevaluate (Add y z) = mlift2 (+) (mevaluate y) (mevaluate z)
mevaluate (Add3 x y z) = (Just add3) `apm` (mevaluate x) `apm` (mevaluate y) `apm` (mevaluate z)
mevaluate (Sub y z) = mlift2 (-) (mevaluate y) (mevaluate z)
mevaluate (Mul y z) = mlift2 (*) (mevaluate y) (mevaluate z)
mevaluate (Div y z) = if isZero (mevaluate z)
                        then Nothing
                        else mlift2 (/)  (mevaluate y) (mevaluate z)


apm :: Maybe (a -> b) -> Maybe a -> Maybe b
apm Nothing _ = Nothing
apm _ Nothing = Nothing
apm (Just f) (Just x) = Just $ f x

mlift2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
mlift2 f = apm . mfmap f

add3 :: (Num a) => a -> a -> a -> a
add3 x y z = x + y + z
