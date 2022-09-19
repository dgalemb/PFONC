--CM5
--entrees, Sorties en Haskell
--inputs and Outputs in Haskell


import Data.Char (ord, chr)
import Data.Maybe (isNothing, fromJust)
import Data.Bitraversable (Bitraversable)
import System.Posix (sigXCPU)
import Distribution.Simple.Utils (xargs)
import Data.Time.Format.ISO8601 (yearFormat)



--fonction pour faire l'inver d'un mot
--function that returns the reversed word
inver [] = [];
inver (x:xs) = inver xs ++ [x];

--on prend l'entree
--getting input
echo = getLine >>= putStrLn 

--la sortie est l'entree plus l'entree inversé
--output = input + tupni
echo1 = getLine >>= (\x -> putStrLn (x ++ " " ++ inver x))

--la sortie est les deux lignes reçus en ordre inversée
--output is the two lines of input in reversed order
echo2 = getLine >>= (\x -> getLine >>= (\y -> putStrLn (y ++ " " ++ x)))

ioLength :: IO Int
ioLength = getLine >>= return.length

cesar :: Int -> String -> String
cesar n = let a = ord 'a' 
        in map((chr.(+a)).('mod' 26)(+n).(subtract a).ord)

execCesar :: IO ()
execCesar = do
    s <- getLine
    n <- getLine
    putStrLn $ cesar (read n) 

loopCesar :: IO ()
loopCesar = do
    s <- getLine
    let x = read n :: Int
    if x >= 1 && x <= 25
        then do 
            s <- getLine
            putStrLn $ cesar x s
        else putStrLn "try again" >> loopCesar


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
        deriving Show 
        
evaluate :: (Fractional a) => Expr a -> a
evaluate (Val x) = x
evaluate (Inc y) = evaluate y + 1
evaluate (Dec y) = evaluate y - 1
evaluate (Neg y) = negate $ evaluate y
evaluate (Inv y) = 1 / evaluate y
evaluate (Add x y) = evaluate x + evaluate y



isZero :: (Eq a, Num a) => Maybe a -> Bool
isZero Nothing = False
isZero (Just x) = (x ==0)

mevaluate :: (Eq a, Fractional a) => Expr a -> Maybe a
mevaluate (Val x) = Just x
mevaluate (Inc y) = let my = mevaluate y
                    in if isNothing my
                          then Nothing
                          else Just $ 1 + fromJust my
mevaluate (Dec y) = let my = mevaluate y
                    in if isNothing my
                          then Nothing
                          else Just $ fromJust my  - 1
mevaluate (Neg y) = let my = mevaluate y
                    in if isNothing my
                          then Nothing
                          else Just $ negate $ fromJust my
mevaluate (Inv y) = let my = mevaluate y
                    in if isNothing my || isZero my
                          then Nothing
                          else Just $ 1 / fromJust my


mfmap :: (a -> b) -> Maybe a -> Maybe b
mfmap _ Nothing = Nothing
mfmap f (Just x) = Just $ f x


mevaluate1 :: (Eq a, Fractional a) => Expr a -> Maybe a
mevaluate1 (Val x) = Just x
mevalueate1 (Inc y) = mfmap (+1) $ mevaluate y
mevalueate1 (Dec y) = mfmap (subtract 1) $ mevaluate y
mevalueate1 (Neg y) = mfmap negate $ mevaluate y
mevalueate1 (Inv y) = let my = mevaluate y
                      in if isZero my then Nothing
                                      else mfmap (1/) my


fmap :: (a -> b) -> [a] -> [b]
fmap _ []  = []
fmap f (x:xs) = f x : fmap f xs

fmap :: (a -> b) -> IO a -> IO b 
fmap :: f x = x >>= (return.f)


fmap :: (a -> b) -> (r -> a) -> (r -> b)
