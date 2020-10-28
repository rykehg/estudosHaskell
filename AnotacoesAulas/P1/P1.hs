module P1 where

{- fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib(n-1) + fib(n-2)


data Tres = Um | Dois | Tres

instance Show Tres where 
  show Um = "1"
  show Dois = "2"
  show Tres = "3"

data Fruta = Abacaxi | Morango | Pessego deriving (Eq, Show)

data Cesta = Cesta Fruta Double deriving (Eq, Show) 

modificarPeso'' :: (Double -> Double) -> [Cesta] -> [Cesta]
  modificarPeso'' f = foldr (\x acc -> (f x) : acc) [] -}

--CORRECAO

--1) fib 5
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib k = fib (k - 1) + fib (k - 2) + fib (k - 5)
--Cai em um caso não tratado de numero negativo logo continua e ai era as respostas da mesma forma


