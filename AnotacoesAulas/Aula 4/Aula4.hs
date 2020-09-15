module Aula4 where
-- CAP 4
-- recursão
-- Tecnica para resolver problemas chmando a propria função
-- A ideia eh diminur o argumento até que chegemos no caso base

-- fatorial porem nao trata negativo
fat :: Int -> Int
fat 0 = 1
fat 1 = 1
fat n = n * fat(n - 1)

-- agora tratando negativo (usamos o Integer para aceitar numeros grandes e nao estourar)
fat2 :: Integer -> Integer
fat2 n
    | n <= 1 = 1
    | otherwise = n * fat2(n-1)

-- Para otimizar usa-se o Tail Recursion Optimization
fat' :: Integer -> Integer -> Integer 
fat' _ 0 = 1
fat' acc 1 = acc
fat' acc n = fat' (n*acc) (n-1)

-- fibonacci
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib(n-1) + fib(n+2)

-- Fib com MEMOIZATION
fib' :: Int -> Int
fib' = ([fibAux k | k <- [0..]] !!)
    where 
      fibAux 0 = 1
      fibAux 1 = 1
      fibAux k = fib' (k-1) + fib'(k-2)

-- Elimina vogal. Ta otimizado? nao, mas o exercicio e usar o tail recursion p/ otimizar a parte da consoante
elimVogal :: String -> String
elimVogal [] = []
elimVogal (x:xs)
  | elem x "AEIOUaeiou" = elimVogal xs
  | otherwise = x : elimVogal xs

-- Lambda -> funcoes anonimas (\p1 p2 ... pn -> EXPRESSAO DE RETORNO)