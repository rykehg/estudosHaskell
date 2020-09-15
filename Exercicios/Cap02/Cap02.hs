module Cap02 where

-- 2.1. Gere as Listas
-- a) [1, 11, 121, 1331, 14641,161051, 1771561]
ex21a :: [Int]
q = 11
ex21a = [q^n | n <- [0 .. 6]]

-- b) [1,2,3,5,6,7,9,10,11,13,14,15,17,18,19,21,22,23,25,26,27,29,30,31,33,34,35,37,38,39]
ex21b :: [Int]
ex21b = [ x | x <- [1 .. 39], x /= 4, x /= 8, x /= 12, x/= 16, x /= 20, x /= 24, x /= 28, x /= 32, x /= 36 ]

ex21bb :: [Int]
ex21bb = [ x | x <- [1 .. 39], x /= [4, 8 .. 36] ]

-- c) ["AaBB",	"AbBB",	"AcBB",	"AdBB",	"AeBB",	"AfBB","AgBB"]
ex21c :: [String]
ex21c = [ 'A' : x : "BB" | x <- ['a' .. 'g']]

-- d) [5,8,11,17,20,26,29,32,38,41]
ex21d :: [Int]
ex21d = [ x | x <- [5, 8 .. 41], x/= 14, x/= 23, x/= 35]

-- e) [1.0,0.5,0.25,0.125,0.0625,0.03125]
ex21e :: [Double]
x = 1.0
ex21e = [ x*1/2^n | n <- [0 .. 5]]

-- f) [1,10,19,28,37,46,55,64]
ex21f :: [Int]
ex21f = [ x | x <- [1, 10 .. 65]]

-- g) [2,4,8,10,12,16,18,22,24,28,30]
ex21g :: [Int]
ex21g = [ x | x<- [2, 4 .. 30]]

-- h) ['@','A','C','D','E','G','J','L']
ex21h :: [Char]
ex21h = [ x | x <- ['@','A'..'L']]

{- 2.2) Crie uma função que verifique se o tamanho de uma String é par ou não. 
          Use Bool como retorno. -}
ex22 :: [Int] -> Bool
ex22 x | ( length x  `mod` 2 == 0) = True | (length x `mod` 2 /= 0) = False

{- 2.3) Escreva uma função que receba um vetor de Strings e retorne uma lista 
          com todos os elementos em ordem reversa. -}
ex23 :: [String] -> [String]
ex23 x = reverse x

{- 2.4) Escreva uma função que receba um vetor de Strings e retorne uma lista 
          com o tamanho de cada String. As palavras de tamanho par devem ser excluídas 
          da resposta. -}
ex24 :: [String] -> [Int]
ex24 x = [length y | y <- x, length y `mod` 2 == 0]

{- 2.5) Escreva a função head como composição de duas outras. -}
ex25 :: String -> Char
ex25 x = (last . reverse) x

{- 2.6) Faça uma função que receba uma String e retorne True se esta for um palíndromo;
          caso contrário, False. -}
reverso :: String -> String
reverso (x:xs) = reverse (xs) ++ [x]

ex26 :: String -> Bool
ex26 (x:xs) = if (x:xs) == reverso (x:xs) then True else False

{- 2.7 Faça uma função que receba um inteiro e retorne uma tupla, contendo: 
          o dobro deste número na primeira coordenada, o triplo na segunda, 
          o quádruplo na terceira e o quíntuplo na quarta. -}
ex27 :: Int -> (Int, Int, Int, Int)
ex27 x = (2*x, 3*x, 4*x, 5*x)