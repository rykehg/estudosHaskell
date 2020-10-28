module Simulado1 where

{- data Resposta = Sim | Nao

instance Show, Eq resposta  -}


foo :: Int -> Int -> Int
foo k 0 = k
foo k x
     | even x = k + (foo (k+1) (x-1)) 
     | otherwise = k * (foo k (x-1))

{- 
even      --> foo 10 4 = 10 + (foo 11 + 3) => 10 + 1705 = 1705
otherwise --> foo 11 3 = 11 * (foo 11 2) => 11 * 155 = 1705
even      --> foo 11 2 = 11 + (foo 12 1) => 11 + 144 = 155
otherwise --> foo 12 1 = 12 * (foo 12 0) => 12 * 12 = 144
foo 12 0 = 12
 -}
