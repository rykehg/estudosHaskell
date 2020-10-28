module Prova where

-- fib 5 = fib 4 + fib 3 + fib 0
-- fib 4 = fib 3 + fib 2 + fib (-1)
fib :: Int -> Int 
fib 0 = 0 
fib 1 = 1 
fib k = fib (k - 1) + fib (k - 2) + fib (k - 5)

{-*
Seja f = (\x y -> 1+max x y)
 foldl f (-99999) [-9,8,2,12,-13,22,75,-67,0] =
 foldl f (f (-99999) (-9)) [8,2,12,-13,22,75,-67,0] =
 foldl f (f (-8) 8) [2,12,-13,22,75,-67,0] =
 foldl f (f 9 2) [12,-13,22,75,-67,0] =
 foldl f (f 10 12) [-13,22,75,-67,0] =
 foldl f (f 13 (-13)) [22,75,-67,0] = 
 foldl f (f 14 22) [75,-67,0] =
 foldl f (f 23 75) [-67,0]
 foldl f (f 76 (-67)) [0] = 
 foldl f (f 77 0) [] =
 f 77 0 = 78
*-}

f1 :: Int -> Int 
f1 x = product [n + 1 | n <- [1..x], n /= 4] 

f2 :: Int -> Int 
f2 x = foldl (*) 1 (map (+1) (filter (/= 4) [1..x]))
 
data Tres = Um | Dois | Tres

instance Show Tres where 
    show Um = "1"
    show Dois = "2"
    show Tres = "3"
    
data Fruta = Abacaxi | Morango | Pessego deriving (Eq, Show)

data Cesta = Cesta Fruta Double deriving (Eq, Show)

modificarPeso :: (Double -> Double) -> [Cesta] -> [Cesta] 
modificarPeso f cs = 
    map (\(Cesta fruta peso) -> Cesta fruta (f peso)) cs

maxFruta :: [Cesta] -> Double 
maxFruta cs = 
    foldl (\acc (Cesta _ p) -> max acc p) (-1) cs

-- data Bool = False | True
-- Either Bool Bool = Bool + Bool = 2 + 2 = 4
--


 
 
