module Aula2 where
-- comentario
{-* comentario multipla linha *-}

dobro :: Int -> Int
dobro x = 2*x

somar :: Int -> Int -> Int 
somar x y = x+y

reverter :: String -> String
reverter ls = reverse ls

dobroSoma :: [Int] -> Int
dobroSoma ns = 2*(sum ns)

tamanhoIns :: Char -> String -> Int
tamanhoIns ch st = length (ch : st)

u :: Int
u = 7

-- u = 8 (não pode atribuir novamente)

-- LIST COMPREHENSIONS
-- LISTA []: EH UMA ESTRUTURA QUE POSSUI A POSSIBILIDADE DE CARREGAR ELEMENTO DE UM TIPO APENAS.
-- PODE SER INFINITAS E SAO DE TAMANHOS VARIADOS
-- EH UM MECANISMO PARA GERAR LISTAS A PARTIR DE UMA EXPRESSAO E FILTROS

--tabuada :: Int -> Int
--tabuada x = [x*n | n <- [1..10]]

-- profa :: [[[Int]]]
-- profa = take 3 (repeat [tabuada m | m <- [1..5]]) -- repete 3 vezes a tabuada de 1 a 5

profa :: [[[Int]]] 
profa = 
    take 8 (repeat [tabuada m | m <- [1..10]])

tabuada :: Int -> [Int] 
tabuada x = [x*n | n <- [1..10]]

divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], mod n x == 0]

ehPrimo :: Int -> Bool
ehPrimo n = 2 == length (divisores n)

-- TUPLA (): ESTRUTURA QUE POSSIBILITA CARREGAR ELEMENTOS DE TIPOS DIFERENTES 
-- SAO FINITO E DE TAMANHO FIXO

somar' :: (Int, Int) -> Int
somar' (x,y) = x+y 
